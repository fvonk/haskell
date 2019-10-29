

--

getRequestUrl host apiKey resource id = host ++ "/" ++ resource ++ "/" ++ id ++ "/" ++ "?token=" ++ apiKey
genHostRequestBuilder host = (\apiKey resource id -> getRequestUrl host apiKey resource id)
genApiRequestBuilder hostBuilder apiKey = (\resource id -> hostBuilder apiKey resource id)
genApiRequestBuilder2 hostBuilder apiKey resource = (\id -> hostBuilder apiKey resource id)
genResourceRequestBuilder apiBuilder resource = (\id -> apiBuilder resource id)

exampleUrlBuilder = genHostRequestBuilder "http://example.com"
myExampleUrlBuilder = genApiRequestBuilder exampleUrlBuilder "sdf2f324234"
myExampleUrlBuilder2 = genApiRequestBuilder2 exampleUrlBuilder "sdf2f00000"
myExampleResourceBuilder = genResourceRequestBuilder  myExampleUrlBuilder "book"


myExampleUrlBuilderResource = getRequestUrl "http://example.com" "sdf2f324234" "book"

flipBinaryArgs f = (\x y -> f y x)
binaryPartialApplication f x = (\y -> f x y)

--
ifEven func x = if even x
  then func x
  else x

genIfEven func = (\x -> ifEven func x)
genIfEvenX x = (\func -> ifEven func x)

-- ifEvenInc = genIfEven inc

--
inc n = n + 1
double n = n * 1
square n = n ^ 2

ifEvenInc = ifEven inc
ifEvenDouble = ifEven double
ifEvenSquare = ifEven square
--
