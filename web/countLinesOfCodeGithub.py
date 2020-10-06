def countLinesOfCode():
    user_input = askForInput()
    while True:
        try:
            code_share = float(user_input)
            break
        except ValueError:
            user_input = input("Enter a valid floating point number!")

    # Now that we have some numeric input. Lets calculate the number of lines of code:
    lines_of_code_of_this_py_file = 100
    lines_of_code = (lines_of_code_of_this_py_file / (code_share / 100))
    print(f'The Github repository consists of {lines_of_code:.0f} lines of code')


def askForInput():
    return input("What percentage of the code is in python. This is displayed on the root directory of the Github repo."
                 " (Assuming that this file is the only file written in python)")


countLinesOfCode()

#----------------------------------------------------------------------------------------------------------------------#
# Whitespace below to ensure that this file has a 100 lines of code
#----------------------------------------------------------------------------------------------------------------------#










































































# This is the 100th line of code