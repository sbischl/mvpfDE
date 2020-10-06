def countLinesOfCode():
    user_input = askForShare()
    while True:
        try:
            code_share = float(user_input)
            break
        except ValueError:
            user_input = input("Enter a valid floating point number!")


    user_input = askForLinesOfCode()
    while True:
        try:
            lines_of_code_of_this_file = int(user_input)
            break
        except ValueError:
            user_input = input("Enter a valid integer!")

    # Now that we have some numeric input. Lets calculate the number of lines of code:
    lines_of_code = (lines_of_code_of_this_file / (code_share / 100))
    print(f'The Github repository consists of {lines_of_code:.0f} lines of code')


def askForShare():
    return input("What percentage of the code is in a programming language for which you know the number of lines of code."
                 " This is displayed on the root directory of the Github repo."
                 " (Assuming that this file is the only file written in python)")

def askForLinesOfCode():
    return input("How many lines are written in this programming language")


countLinesOfCode()