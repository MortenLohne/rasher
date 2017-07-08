use board::std_board::Square;

#[test]
fn from_alg_test() {
    assert_eq!(Square::from_alg("a8").unwrap(), Square(0));
    assert_eq!(Square::from_alg("b8").unwrap(), Square(1));
    assert_eq!(Square::from_alg("g1").unwrap(), Square(62));
}
