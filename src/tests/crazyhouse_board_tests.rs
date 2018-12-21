use tests::board_tests;
use board::crazyhouse_board::CrazyhouseBoard;
use search_algorithms::board::Board;

#[test]
fn san_lan_test() {
    for _ in 0..10 {
        board_tests::test_san_lan_with_random_game(CrazyhouseBoard::start_board());
    }
}