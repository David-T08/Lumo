// Used to auto implement the Display trait

#[macro_export]
macro_rules! auto_display_enum {
  (
      $(#[$outer:meta])* // To allow attributes like #[derive(Debug)] on the enum
      $vis:vis enum $enum_name:ident {
          $(
              $(#[$inner:meta])* // To allow attributes on variants if ever needed (e.g., #[doc = "..."])
              $variant:ident => $symbol:literal
          ),* $(,)? // $(,)? allows an optional trailing comma
      }
  ) => {
      $(#[$outer])*
      $vis enum $enum_name {
          $(
              $(#[$inner])*
              $variant,
          )*
      }

      impl std::fmt::Display for $enum_name {
          fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
              use $enum_name::*;
              let symbol_str = match self {
                  $(
                      $variant => $symbol,
                  )*
              };
              write!(f, "{}", symbol_str)
          }
      }
  };
}