from distutils.dir_util import copy_tree

def copyFiles(from_folder, to_folder):
    working_directory = "C:/Users/Simon/IdeaProjects/UnifiedWelfareGer/"
    target_directory = "C:/Users/Simon/OneDrive/Universität/Fächer/Master Thesis/Latex/"
    copy_tree(working_directory + from_folder, target_directory + to_folder)

copyFiles("plots", "figures")
copyFiles("tex", "tables")