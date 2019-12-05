# memo

- `Indexed Monads` の日本語訳としては `指標付きモナド` が唯一観測された。
- `ibind :: (a -> m j k b) -> m i j a -> m i k b`
  - `i j`, `j k`, `i k` のような感じ