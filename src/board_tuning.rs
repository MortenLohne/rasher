use search_algorithms::board::ExtendedBoard;
use search_algorithms::board::GameResult;
use search_algorithms::alpha_beta::Score;
use search_algorithms::monte_carlo;
use pgn::PgnBoard;
use rayon::prelude::*;

pub trait TunableBoard {
    fn static_eval_with_params(&self, params: &[f32]) -> f32;
}

pub fn error<B>(positions: &mut [(B, GameResult)], params: &[f32]) -> f32
    where B: TunableBoard + ExtendedBoard + PgnBoard + Send {
    positions.into_par_iter()
        .map(|(board, game_result)| {
            let color = board.side_to_move();
            if let Score::Val(qsearc_eval) = qsearch(&mut board.clone(), Score::Loss(0),
                                                     Score::Win(0), params) {
                let eval = qsearc_eval * color.multiplier() as f32;

                let answer = match game_result {
                    GameResult::WhiteWin => 1.0,
                    GameResult::Draw => 0.5,
                    GameResult::BlackWin => 0.0,
                };

                let error = f32::powf(answer - monte_carlo::eval_to_win_pct(eval), 2.0);
                error
            }
            else {
                warn!("Found mate score while tuning {}.", board.to_fen());
                0.0
            }

    })
        .sum()
}

fn qsearch<B: ExtendedBoard>(board: &mut B, mut alpha: Score, beta: Score, params: &[f32]) -> Score
    where B: TunableBoard + ExtendedBoard {
    let color = board.side_to_move();

    if let Some(result) = board.game_result() {
        return Score::from_game_result(result, color)
    }

    let stand_pat = Score::Val(board.static_eval_with_params(params) * color.multiplier() as f32);

    if stand_pat >= beta {
        return beta;
    }

    alpha = alpha.max(stand_pat);

    let mut active_moves = vec![];
    board.active_moves(&mut active_moves);


    for mv in active_moves {
        let reverse_move = board.do_move(mv);

        let mut score = qsearch(board, beta, alpha, params);

        score = !score;
        board.reverse_move(reverse_move);

        if score >= beta {
            alpha = score; // TODO: Could be beta as well. Test.
            break;
        }
        if score > alpha {
            alpha = score;
        }
    }

    return alpha;

}