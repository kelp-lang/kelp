const ESC: &str = "\u{001b}[";
const OSC: &str = "\u{001b}]";
const BEL: &str = "\u{0007}";
const SEP: &str = ";";

pub fn link(url: &str, text: &str) -> String {
    let arr = [
        OSC,
        "8",
        SEP,
        SEP,
        url,
        BEL,
        text,
        OSC,
        "8",
        SEP,
        SEP,
        BEL,
    ];

    arr.concat()
}
