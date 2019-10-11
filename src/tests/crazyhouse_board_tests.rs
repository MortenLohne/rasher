use board::crazyhouse::board::CrazyhouseBoard;
use board_game_traits::board::Board;
use tests::tools;

#[test]
fn san_lan_test() {
    for _ in 0..10 {
        tools::test_san_lan_with_random_game(CrazyhouseBoard::start_board());
    }
}