use uci;
use uci::TimeRestriction;
use uci::TimeInfo;
use std::time::Duration;

#[test]
fn parse_go_test() {
    

    assert_eq!(uci::parse_go("go infinite").map(|r|r.0), Ok(TimeRestriction::Infinite));
    assert_eq!(uci::parse_go("go movetime 5000").map(|r|r.0), 
               Ok(TimeRestriction::MoveTime(Duration::from_millis(5000))));
    assert_eq!(uci::parse_go("go wtime 1000 btime 2500 winc 54 binc 22").map(|r|r.0), 
           Ok(TimeRestriction::GameTime(TimeInfo {
               white_time: Duration::from_millis(1000), black_time: Duration::from_millis(2500), 
               white_inc: Duration::from_millis(54), black_inc: Duration::from_millis(22) ,
               moves_to_go: None }
           )));
    
}
