use crate::js::data::intermediate::CodeLoc;

pub struct ColLineMap {
    lines: Vec<usize>,
    columns: Vec<usize>,
}

impl ColLineMap {
    pub fn new(source: &str) -> ColLineMap {
        let mut cur_col = 1;
        let mut cur_line = 1;

        let mut lines = Vec::new();
        let mut columns = Vec::new();

        for byte in source.as_bytes().iter() {
            if (*byte) == '\n' as u8 {
                cur_col = 1;
                cur_line += 1;
            }
            lines.push(cur_line);
            columns.push(cur_col);
        }
        return ColLineMap { lines, columns };
    }

    pub fn loc_for(&self, low_byte: usize) -> CodeLoc {
        return CodeLoc {
            line: self.lines.get(low_byte).map(|a| *a).unwrap_or(0),
            column: self.columns.get(low_byte).map(|a| *a).unwrap_or(0),
        };
    }
}
