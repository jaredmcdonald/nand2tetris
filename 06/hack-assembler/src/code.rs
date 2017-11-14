use std::collections::HashSet;

fn dest(mnemonic: Option<&str>) -> [u8; 3] {
    match mnemonic {
        Some(content) => {
           // a set is probably overkill here, oh well
            let dest_set = content.split("").collect::<HashSet<&str>>();
            [
                dest_set.contains("A") as u8,
                dest_set.contains("D") as u8,
                dest_set.contains("M") as u8,
            ]
        },
        None => [0, 0, 0],
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_dest() {
        assert_eq!(dest(None), [0, 0, 0]);
        assert_eq!(dest(Some("AMD")), [1, 1, 1]);
        assert_eq!(dest(Some("A")), [1, 0, 0]);
        assert_eq!(dest(Some("D")), [0, 1, 0]);
        assert_eq!(dest(Some("M")), [0, 0, 1]);
        assert_eq!(dest(Some("AD")), [1, 1, 0]);
        assert_eq!(dest(Some("DM")), [0, 1, 1]);
        assert_eq!(dest(Some("AM")), [1, 0, 1]);
    }
}
