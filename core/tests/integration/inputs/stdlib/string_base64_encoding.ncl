# test.type = 'pass'
let { string, .. } = std in

[
  # string.base64_encode
  string.base64_encode 'Standard "Hello, world!" == "SGVsbG8sIHdvcmxkIQ==",
  string.base64_encode 'NoPad "Hello, world!" == "SGVsbG8sIHdvcmxkIQ",
  string.base64_encode 'Standard "😎" == "8J+Yjg==",
  string.base64_encode 'UrlSafe "😎" == "8J-Yjg==",
  string.base64_encode 'UrlSafeNoPad "😎" == "8J-Yjg",
  # string.base64_encode
  string.base64_decode 'Standard "SGVsbG8sIHdvcmxkIQ==" == "Hello, world!",
  string.base64_decode 'NoPad "SGVsbG8sIHdvcmxkIQ" == "Hello, world!",
  string.base64_decode 'Standard "8J+Yjg==" == "😎",
  string.base64_decode 'UrlSafe "8J-Yjg==" == "😎",
  string.base64_decode 'UrlSafeNoPad "8J-Yjg" == "😎",
]
|> std.test.assert_all
