module SVGWriter where

svgHeader :: [String]
svgHeader = [ "<?xml version=\"1.0\" standalone=\"no\"?>",
              "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"",
              "\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">" ]

data SVGElem = Line { x1 :: Float, y1 :: Float, x2 :: Float, y2 :: Float}
             | Rect { x :: Float, y :: Float,
                      rwidth :: Float, rheight :: Float }

data SVG = SVG { width :: Float, height :: Float, elems :: [SVGElem]}

selfClosingTag :: String -> [(String, String)] -> String
selfClosingTag name ps =
  let attrString (attr, value) = attr ++ "=\"" ++ value ++  "\""
      attrs = map attrString ps
  in "<" ++ name ++ " " ++ unwords attrs ++ " />"

writeElem :: SVGElem -> String
writeElem (Line a b c d) =
  let attrs = zip ["x1","y1","x2","y2"] (map show [a,b,c,d])
      disp  = [("stroke","black"), ("stroke-width","0.01")]
  in selfClosingTag "line" (attrs ++ disp)
writeElem (Rect a b c d) =
  let attrs = zip ["x","y","width","height"] (map show [a,b,c,d])
      disp  = [("stroke","black"), ("stroke-width","1"), ("fill","none")]
  in selfClosingTag "rect" (attrs ++ disp)

writeSVG :: SVG -> String
writeSVG (SVG w h es) =
  let view = unwords (map show [0, 0, w, h])
      header = [ "<svg width=\"8cm\" height=\"8cm\" viewBox=\"" ++ view ++ "\"",
               "xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">" ]
      body = map writeElem es
      footer = ["</svg>"]
  in unlines (svgHeader ++ header ++ body ++ footer)

