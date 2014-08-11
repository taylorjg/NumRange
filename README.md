
## Description

The idea of this little project is to come up with some C# code that takes a string like <code>"1-3,6,9"</code> and returns an <code>IEnumerable&lt;int&gt;</code> that contains <code>1,2,3,6,9</code>.

## Haskell version

I originally came up with the following Haskell function (I am fairly new to Haskell so this probably isn't great code) and wanted to see what the equivalent C# would look like.

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

## Links

* [Code Golf](http://en.wikipedia.org/wiki/Code_golf)
