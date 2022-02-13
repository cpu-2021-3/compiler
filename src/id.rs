use global_counter::primitive::exact::CounterI32;

static ID_COUNTER: CounterI32 = CounterI32::new(0);

/// 今までに生成されていない新しい ID を作る
/// α 変換などに使用
pub fn generate_id(s: &str) -> String {
    ID_COUNTER.inc();
    let s = &s[..s.find('.').unwrap_or(s.len())];
    s.to_string() + "." + &ID_COUNTER.get().to_string()
}
