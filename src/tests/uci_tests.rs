
#[test]
fn parse_go_test() {
    use board::std_board::*;
    use uci;
    use uci::TimeRestriction;

    assert_eq!(uci::parse_go("go infinite").map(|r|r.0), Ok(TimeRestriction::Infinite));
    assert_eq!(uci::parse_go("go movetime 5000").map(|r|r.0), 
               Ok(TimeRestriction::MoveTime(5000)));
    assert_eq!(uci::parse_go("go wtime 1000 btime 2500 winc 54 binc 22").map(|r|r.0), 
           Ok(TimeRestriction::GameTime(TimeInfo {
               white_time: 1000, black_time: 2500, 
               white_inc: 54, black_inc: 22 , moves_to_go: None }
                                        )));
    
}
