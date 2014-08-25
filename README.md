
## Description

The idea of this little project is to come up with some C# code that takes a string like <code>"1-3,6,9"</code> and returns an <code>IEnumerable&lt;int&gt;</code> that contains <code>1,2,3,6,9</code>.

## Haskell version

I originally came up with the following Haskell function (I am fairly new to Haskell so this probably isn't great code) and wanted to see what the equivalent C# code would look like.

```Haskell
import Data.List.Split (wordsBy)

stringToNumList :: String -> [Int]
stringToNumList s =
	foldl loop [] $ wordsBy (==',') s
	where loop acc w =
		acc ++ case map read $ wordsBy (=='-') w of
			(a:b:_) -> [a..b]
			(a:_) -> [a]

main :: IO ()
main = do
	let input = "1-12,14,16"
	putStrLn $ show $ stringToNumList input
```

### Update V1

A slight simplification of my original Haskell code using <code>(>>=)</code> for lists instead of <code>foldl</code>. The list concatenation now occurs inside <code>(>>=)</code>.

```Haskell
import Data.List.Split (wordsBy)

stringToNumList :: String -> [Int]
stringToNumList s =
	wordsBy (==',') s >>= \w ->
		case map read $ wordsBy (=='-') w of
			a:b:[] -> [a..b]
			a:[] -> return a

main :: IO ()
main = do
	let input = "1-12,14,16"
	putStrLn $ show $ stringToNumList input
```

### Update V2

Tweak to the pattern matching in the case expression.

```Haskell
import Data.List.Split (wordsBy)

stringToNumList :: String -> [Int]
stringToNumList s =
    wordsBy (==',') s >>= \w ->
        case map read $ wordsBy (=='-') w of
            [a, b] -> [a..b]
            [a] -> [a]

main :: IO ()
main = do
    let input = "1-12,14,16"
    putStrLn $ show $ stringToNumList input
```

## Links

* [Code Golf](http://en.wikipedia.org/wiki/Code_golf)
