module Strings
(
 intro,
 please,
 options,
 example,
 difficulty,
 displayDif
) where


intro = "Welcome to Hidato solver!!! \n\nPress Ctrl+D anytime to exit\n"
please = "Please select an option: \n"
options = ["1-) Write an Hidato to be solved",
           "2-) Generate a random Hidato"]
displayDif = ["pie",
              "easy",
              "normal",
              "hard",
              "insane\n"]

example = "0 6 -\n10 0 7\n9 8 0\n1 0 -\n"


difficulty :: [(String, Integer)]
difficulty = [ ("pie", 10),
               ("easy", 30),
               ("normal", 40),
               ("hard", 50),
               ("insane", 60)
              ]