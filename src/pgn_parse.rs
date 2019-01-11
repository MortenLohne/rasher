use pgn::PgnBoard;
use std::io::Write;
use search_algorithms::board::GameResult;
use std::io;
use search_algorithms::board::Color;
use search_algorithms::board::Board;
use std::error;
use std::fmt::Debug;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Game<B: Board> {
    pub start_board: B,
    pub moves: Vec<(B::Move, String)>,
    pub tags: Vec<(String, String)>,
}

pub fn game_to_pgn<W: Write, B: PgnBoard>(board: &mut B, moves: &[(B::Move, String)], event: &str, site: &str, date: &str,
                         round: &str, white: &str, black: &str, result: Option<GameResult>,
                         tags_pairs: &[(&str, &str)], f: &mut W) -> Result<(), io::Error> {
    writeln!(f, "[Event \"{}\"]", event)?;
    writeln!(f, "[Site \"{}\"]", site)?;
    writeln!(f, "[Date \"{}\"]", date)?;
    writeln!(f, "[Round \"{}\"]", round)?;
    writeln!(f, "[White \"{}\"]", white)?;
    writeln!(f, "[Black \"{}\"]", black)?;
    writeln!(f, "[Result \"{}\"]", match result {
        None => "*",
        Some(GameResult::WhiteWin) => "1-0",
        Some(GameResult::BlackWin) => "0-1",
        Some(GameResult::Draw) => "1/2-1/2",
    })?;

    if tags_pairs.iter().find(|(tag, _)| *tag == "FEN" ).is_none()
        && *board != B::start_board() {
        writeln!(f, "[FEN \"{}\"", board.to_fen())?;
    }

    for (i, (mv, comment)) in moves.into_iter().enumerate() {
        if i % 12 == 0 {
            writeln!(f)?;
        }
        if i == 0 && board.side_to_move() == Color::Black {
            write!(f, "{}... {} {{{}}} ", 1, board.move_to_san(&mv), comment)?;
        }
        else if board.side_to_move() == Color::White {
            write!(f, "{}. {} {{{}}} ", (i + 1) / 2 + 1, board.move_to_san(&mv), comment)?;
        }
        else {
            write!(f, "{} {{{}}} ", board.move_to_san(&mv), comment)?;
        }
        board.do_move(mv.clone());
    }

    write!(f, "{}", match result {
        None => "*",
        Some(GameResult::WhiteWin) => "1-0",
        Some(GameResult::BlackWin) => "0-1",
        Some(GameResult::Draw) => "1/2-1/2",
    })?;
    writeln!(f)?;
    writeln!(f)?;
    Ok(())
}

pub fn parse_pgn<B: PgnBoard + Debug + Clone>(mut input: &str) -> Result<Vec<Game<B>>, Box<dyn error::Error>> {

    let mut games = vec![];

    loop {
        let result = parse_game(input);
        match result {
            Ok((rem_input, (tag_pairs, move_texts))) => {
                let mut board = B::start_board();
                let mut moves = vec![];
                for (ref move_text, ref comment) in move_texts.iter() {
                    let mv = board.move_from_san(move_text);
                    match mv {
                        Err(err) => {
                            println!("{:?}\n{}", board, err);
                            return Err(err.into());
                        },
                        Ok(mv) => {
                            board.do_move(mv.clone());
                            moves.push((mv, comment.unwrap_or("").to_string()));
                        }
                    }

                }

                input = rem_input;

                let game = Game {
                    start_board: B::start_board(),
                    moves: moves,
                    tags: tag_pairs.iter().map(|(a, b)| (a.to_string(), b.to_string())).collect(),
                };
                // println!("{:?}", game.clone());

                games.push(game);
            },
            Err(err) => {
                match err {
                    nom::Err::Incomplete(i) =>
                        write!(io::stderr(), "Couldn't parse incomplete game: {:?}\n", i)?,
                    nom::Err::Error(nom::Context::Code(i, error_kind)) =>
                        write!(io::stderr(), "Parse error of kind {:?} around {}\n", error_kind, &i[0..100])?,
                    nom::Err::Error(nom::Context::List(errs)) =>
                        write!(io::stderr(), "Parse error: {:?}\n", errs)?,
                    nom::Err::Failure(nom::Context::Code(i, error_kind)) =>
                        write!(io::stderr(), "Parse failure of kind {:?} around {}\n", error_kind, i)?,
                    nom::Err::Failure(nom::Context::List(errs)) =>
                        write!(io::stderr(), "Parse failure: {:?}\n", errs)?,
                }
                break;
            },
        }
    }

    Ok(games)
}

named!(parse_game<&str, (Vec<(&str, &str)>, Vec<(&str, Option<&str>)>)>,
    do_parse!(
        tag_pairs: parse_tag_pairs >>
        many0!(alt!(tag!(" ") | tag!("\n"))) >>
        moves: parse_game_movetext >>
        many0!(alt!(tag!(" ") | tag!("\n"))) >>
        ((tag_pairs, moves))
    )
);


named!(parse_game_movetext<&str, Vec<(&str, Option<&str>)>>,
    do_parse!(
        result: complete!(dbg!(
        many_till!(parse_move_text,
            alt!(tag!("0-1") | tag!("1-0") | tag!("1/2-1/2") | tag!("*"))
        ))) >>
        (result.0)
    )
);

named!(parse_move_text<&str, (&str, Option<&str>)>,
    do_parse!(
        dbg!(opt!(complete!(many_till!(nom::digit, alt!(tag!("... ") | tag!(". ")))))) >>
        movetext: return_error!(dbg!(complete!(take_until!(" ")))) >>
        comment: opt!(complete!(do_parse!(
                many1!(alt!(tag!(" ") | tag!("\n"))) >>
                char!('{') >>
                comment: take_until!("}") >>
                char!('}') >>
                (comment)
            ))
        ) >>
        many1!(complete!(alt!(tag!(" ") | tag!("\n")))) >>
        (movetext, comment)
    )
);

named!(parse_tag_pairs<&str, Vec<(&str, &str)>>,
    many0!(do_parse!(
        tag: parse_tag_pair >>
        tag!("\n") >>
        (tag)
    ))
);

named!(parse_tag_pair<&str, (&str, &str)>,
    do_parse!(
        char!('[') >>
        name: take_until_and_consume!(" ") >>
        char!('"') >>
        value: take_until_and_consume!("\"]") >>
        ((name, value))
    )
);