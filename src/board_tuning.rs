use search_algorithms::board::ExtendedBoard;
use search_algorithms::board::GameResult;
use search_algorithms::alpha_beta::Score;
use search_algorithms::monte_carlo;
use pgn::PgnBoard;
use rayon::prelude::*;

pub trait TunableBoard {
    const PARAMS: &'static [f32];

    fn static_eval_with_params(&self, params: &[f32]) -> f32;
}

pub fn get_critical_positions<B>(positions: &mut [B], params: &[f32]) -> Vec<B>
    where B: TunableBoard + ExtendedBoard + PgnBoard + Send {
    positions.into_par_iter()
        .map(|board| {
            if let (Score::Val(_), pv) = qsearch(&mut board.clone(), Score::Loss(0),
                                     Score::Win(0), params) {
                let mut cloned_board = board.clone();

                for mv in pv {
                    cloned_board.do_move(mv);
                }
                cloned_board
            }
            else {
                warn!("Found mate score while tuning {}.", board.to_fen());
                board.clone()
            }

        })
        .collect()
}

pub fn gradient_descent<B>(positions: &mut [B], results: &[GameResult],
                           test_positions: &mut[B], test_results: &[GameResult],
                           params: &[f32]) -> Vec<f32>
    where B: TunableBoard + ExtendedBoard + PgnBoard + Send {
    const ETA: f32 = 0.1;

    let mut errors = vec![std::f32::MAX];
    let mut paramss: Vec<Vec<f32>> = vec![params.to_vec()];

    loop {
        let last_params = paramss.last().unwrap().clone();
        let gradients = gradients(positions, results, &last_params);
        println!("Gradients: {:?}", gradients);

        let new_params: Vec<f32> = last_params.iter()
            .zip(gradients)
            .map(|(param, grad)| param + grad * ETA)
            .collect();
        println!("New parameters: {:?}", new_params);

        let error = average_error(test_positions, test_results, &new_params);
        println!("Error now {}\n", error);

        if !errors.len() > 1 && error > errors[errors.len() - 1] && error > errors[errors.len() - 2] {
            if paramss[paramss.len() - 1] < paramss[paramss.len() - 2] {
                return paramss[paramss.len() - 1].clone()
            }
            else {
                return paramss[paramss.len() - 2].clone()
            }
        }
        else {
            errors.push(error);
            paramss.push(new_params);
        }
    }
}

pub fn gradients<B>(positions: &mut [B], results: &[GameResult], params: &[f32]) -> Vec<f32>
    where B: TunableBoard + ExtendedBoard + PgnBoard + Send {

    let critical_positions = get_critical_positions(positions, params);

    const EPSILON : f32 = 0.001;

    params.iter().enumerate()
        .map(|(i, p)| {
            let mut params_hat: Vec<f32> = params.iter().cloned().collect();
            params_hat[i] = p + EPSILON;
            critical_positions.iter()
                .zip(results)
                .map(|(board, &game_result)| {
                    let score1 = board.static_eval_with_params(params);
                    let score2 = board.static_eval_with_params(&params_hat);
                    error(score1, game_result) - error(score2, game_result)
                })
                .sum()
        })
        .collect()
}

pub fn average_error<B>(positions: &mut [B], results: &[GameResult], params: &[f32]) -> f32
    where B: TunableBoard + ExtendedBoard + PgnBoard + Send {
    positions.into_par_iter().zip(results)
        .map(|(board, game_result)| {
            let color = board.side_to_move();
            if let (Score::Val(qsearc_eval), _) = qsearch(&mut board.clone(), Score::Loss(0),
                                                     Score::Win(0), params) {
                let eval = qsearc_eval * color.multiplier() as f32;
                error(eval, game_result.clone())

            }
            else {
                warn!("Found mate score while tuning {}.", board.to_fen());
                0.0
            }
        })
        .sum::<f32>() / (positions.len() as f32)

}

pub fn error(eval: f32, game_result: GameResult) -> f32 {
    let answer = match game_result {
        GameResult::WhiteWin => 1.0,
        GameResult::Draw => 0.5,
        GameResult::BlackWin => 0.0,
    };

    let error = f32::powf(answer - monte_carlo::eval_to_win_pct(eval), 2.0);
    error
}

fn qsearch<B: ExtendedBoard>(board: &mut B, mut alpha: Score, beta: Score, params: &[f32])
    -> (Score, Vec<B::Move>)
    where B: TunableBoard + ExtendedBoard {
    let color = board.side_to_move();

    if let Some(result) = board.game_result() {
        return (Score::from_game_result(result, color), vec![])
    }

    let stand_pat = Score::Val(board.static_eval_with_params(params) * color.multiplier() as f32);

    if stand_pat >= beta {
        return (beta, vec![]);
    }

    alpha = alpha.max(stand_pat);
    let mut best_pv = vec![];
    let mut best_move = None;

    let mut active_moves = vec![];
    board.active_moves(&mut active_moves);

    for mv in active_moves {
        let reverse_move = board.do_move(mv.clone());

        let (mut score, mut pv) = qsearch(board, beta, alpha, params);

        score = !score;
        board.reverse_move(reverse_move);

        if score >= beta {
            return (score, vec![mv]);
        }
        if score > alpha {
            alpha = score;
            best_pv = pv;
            best_move = Some(mv);
        }
    }
    best_pv.extend(best_move);
    return (alpha, best_pv);

}