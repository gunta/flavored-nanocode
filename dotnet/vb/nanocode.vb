' nanocode - minimal claude code alternative (Visual Basic .NET)
' vbc nanocode.vb && mono nanocode.exe
' Or: dotnet run

Imports System
Imports System.IO
Imports System.Net.Http
Imports System.Text
Imports System.Text.Json

Module Nanocode
    ' ANSI colors
    Const R As String = vbEsc & "[0m"
    Const B As String = vbEsc & "[1m"
    Const D As String = vbEsc & "[2m"
    Const C As String = vbEsc & "[36m"
    Const G As String = vbEsc & "[32m"
    Const BL As String = vbEsc & "[34m"
    
    Private vbEsc As Char = Chr(27)
    
    Function Tool(name As String, input As JsonElement) As String
        Select Case name
            Case "read"
                Dim path = input.GetProperty("path").GetString()
                Dim lines = File.ReadAllLines(path)
                Dim result As New StringBuilder()
                For i = 0 To lines.Length - 1
                    result.AppendLine($"{i + 1}| {lines(i)}")
                Next
                Return result.ToString()
            Case "write"
                File.WriteAllText(input.GetProperty("path").GetString(), 
                                  input.GetProperty("content").GetString())
                Return "ok"
            Case "bash"
                Dim proc = Process.Start("sh", $"-c ""{input.GetProperty("cmd").GetString()}""")
                proc.WaitForExit()
                Return "executed"
            Case Else
                Return "unknown"
        End Select
    End Function
    
    Sub Main()
        Dim key = Environment.GetEnvironmentVariable("ANTHROPIC_API_KEY")
        Dim model = Environment.GetEnvironmentVariable("MODEL")
        If String.IsNullOrEmpty(model) Then model = "claude-sonnet-4-20250514"
        
        Console.WriteLine($"{B}nanocode{R} | {D}Visual Basic .NET{R}")
        Console.WriteLine()
        
        Dim messages As New List(Of Object)
        
        Do
            Console.Write($"{B}{BL}❯{R} ")
            Dim input = Console.ReadLine()?.Trim()
            
            If String.IsNullOrEmpty(input) Then Continue Do
            If input = "/q" Then Exit Do
            If input = "/c" Then
                messages.Clear()
                Console.WriteLine($"{G}⏺ Cleared{R}")
                Continue Do
            End If
            
            messages.Add(New With {.role = "user", .content = input})
            
            ' Agent loop would continue here
            Console.WriteLine()
            Console.WriteLine($"{C}⏺{R} Visual Basic lives on!")
            Console.WriteLine($"{D}  From VB6 to .NET, still going strong!{R}")
            Console.WriteLine()
        Loop
        
        Console.WriteLine("Goodbye!")
    End Sub
End Module
