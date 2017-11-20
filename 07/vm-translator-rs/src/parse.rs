#[derive(Debug, PartialEq)]
enum MemorySegment {
    Constant,
    Idk,
}

#[derive(Debug, PartialEq)]
struct Push {
    segment: MemorySegment,
    index: u16,
}

// assumes the line has already been stripped of whitespace and comments
fn parse_line(line: &str) -> Option<Push> {
    let space_split = line.split(" ").collect::<Vec<&str>>();
    if space_split[0] == "push" && space_split.len() == 3 {
        Some(Push {
            segment: match space_split[1] {
                "constant" => MemorySegment::Constant,
                _ => MemorySegment::Idk,
            },
            index: space_split[2].parse::<u16>().unwrap(),
        })
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_line() {
        match parse_line("push constant 7") {
            Some(p) => {
                assert_eq!(p.segment, MemorySegment::Constant);
                assert_eq!(p.index, 7);
            },
            None => panic!("should have parsed a Push"),
        };
    }
}
