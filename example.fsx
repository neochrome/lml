#load "lml.fsx"

let doc = 
    T?html <- [
        T?head <- [
            T?style <- "body { color:red;}"
        ];
        T?body <- 
            T?div <- ["id"=>"something"; "class"=>"edit"; &"TextContent"]
    ]
doc.write
