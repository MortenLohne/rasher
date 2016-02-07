
/// This function takes a GameState and a function 
fn monte_carlo_search<GS, M, F, G> (game_state : GS,
                                 legal_moves : F, do_move : G, is_won : fn(GS) -> bool
                                 ) -> !
    where F : Fn(GS) -> Vec<M>, G : Fn(GS, M) -> GS {
        let mut move_scores = Vec::new();
        for g_move in legal_moves(game_state) {
            move_scores.push((g_move, 0, 0));
        }
        loop {
            
        }
    }
