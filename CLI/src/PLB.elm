module PLB exposing (..)

import Generic.Print
import MicroLaTeX.PrimitiveBlock exposing (parse)
import Posix.IO as IO exposing (IO, Process)
import Posix.IO.File as File
import Posix.IO.Process as Proc


program : Process -> IO ()
program process =
    case process.argv of
        [ _, filename ] ->
            IO.do
                (File.contentsOf filename
                    |> IO.exitOnError identity
                )
            <|
                \content ->
                    let
                        parsed =
                            content |> String.lines |> parse "id" 0

                        blockString =
                            "\n----------------\nBLOCKS\n----------------\n\n"
                                ++ (List.map Generic.Print.print parsed |> String.join "\n\n")
                    in
                    IO.do (Proc.print blockString) <|
                        \_ ->
                            IO.return ()

        _ ->
            Proc.logErr "Usage: elm-cli <program> file\n"
