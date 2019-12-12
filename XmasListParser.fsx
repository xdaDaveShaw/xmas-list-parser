#r "FParsec\\FParsecCS.dll"
#r "FParsec\\FParsec.dll"

open FParsec

module Domain =
    type Behaviour = Naughty | Nice

    type Gift = {
        Gift: string
        Quantity: int
    }

    type Child = {
        Name: string
        Behaviour: Behaviour
        Gifts: Gift list
    }

module Parser = 

    type Line = 
        | Child of string * Domain.Behaviour
        | QuantifiedGift of string * int
        | SingleGift of string

    //Helper function for Whitespace around a single char.
    let wsAround c = 
        spaces >>. skipChar c >>. spaces

    let startOfGiftName = wsAround '-'

    let pQuantifiedGift = 
        let endOfQty = wsAround '*'
        let pGiftName = 
            startOfGiftName >>. manyCharsTill anyChar endOfQty        
        pGiftName .>>. pint32 |>> QuantifiedGift

    let pSingleGift = 
        let allTillEOL = manyChars (noneOf "\n")
        startOfGiftName >>. allTillEOL |>> SingleGift

    let pChild = 
        let pName =
            let endOfName = wsAround ':'
            many1CharsTill anyChar endOfName |>> string

        let pBehaviour = 
            (pstringCI "nice" >>% Domain.Nice)
            <|>
            (pstringCI "naughty" >>% Domain.Naughty)

        pName .>>. pBehaviour |>> Child

    let pLine =
        attempt pQuantifiedGift
        <|>
        attempt pSingleGift
        <|>
        pChild

    let parseInput input =
        run (sepBy pLine newline) input

module Translation = 

    open Domain
    open Parser

    let foldLine line state = 

        let cList, gList = state

        let addChild name behaviour = 
            { Name = name; Behaviour = behaviour; Gifts = gList; } :: cList

        let addGift name quantity = 
            { Gift = name; Quantity = quantity; } :: gList

        match line with
        | Child (name, behaviour) -> addChild name behaviour, []
        | SingleGift name -> cList, addGift name 1
        | QuantifiedGift (name, quantity) -> cList, addGift name quantity

    let mapLinesToDomain lines = 
        let initState = [],[]

        let mapped = 
            match lines with
            | Success (lines, _, _) -> Seq.foldBack foldLine lines initState
            | Failure (err, _, _) -> failwith err

        fst mapped

module Program = 

    open System.IO

    let input = File.ReadAllText("examples.txt").Trim()

    let nl = System.Environment.NewLine
    let formatGifts (gifts : Domain.Gift list) = 
        gifts
        |> Seq.map (fun gift -> sprintf "%s * %d" gift.Gift gift.Quantity)
        |> String.concat nl

    let printChild (child: Domain.Child) = 
        printfn "%s: %A%s%s" child.Name child.Behaviour nl (formatGifts child.Gifts)

    let main() =
        Parser.parseInput input 
        |> Translation.mapLinesToDomain 
        |> Seq.iter printChild

Program.main()