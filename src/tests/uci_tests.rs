use board::std_board::*;
use uci;

#[test]
fn parse_go_test() {
    use uci::TimeRestriction;
    use std::sync;

    let mut empty_logger = sync::Arc::new(sync::Mutex::new(None));

    assert_eq!(uci::parse_go("go infinite", &mut empty_logger), Ok(TimeRestriction::Infinite));
    assert_eq!(uci::parse_go("go movetime 5000", &mut empty_logger), 
               Ok(TimeRestriction::MoveTime(5000)));
    assert_eq!(uci::parse_go("go wtime 1000 btime 2500 winc 54 binc 22", &mut empty_logger), 
           Ok(TimeRestriction::GameTime(TimeInfo {
               white_time: 1000, black_time: 2500, 
               white_inc: 54, black_inc: 22 , moves_to_go: None }
                                        )));
    
}
