def read_lines(path2):
    if path2 == "":
        return ["a", "b"]
    return ["a", "b"]

def get_owner(line):
    i = line.find("foo")
    return None if i == -1 else line[i:].split()[0]    

def foo(path):
    for line in read_lines(path):
        s = get_owner(line)

foo("")



# import os


# def _read_lines(path, remove_comment_lines):
#     lines = []
#     with open(path) as f:
#         for line in f.readlines():
#             line = line.strip()
#             skip_line = False
#             if remove_comment_lines and line.startswith("#"):
#                 skip_line = True
#             if not skip_line:
#                 lines.append(line)
#     return lines


# def _get_primary_owning_team(codeowners_line):
#     TEAM_PREFIX = "@org1/"
#     i = codeowners_line.find(TEAM_PREFIX)
#     return None if i == -1 else codeowners_line[i:].split()[0]


# def _read_CODEOWNERS_lines(repo_root, remove_comment_lines):
#     return _read_lines(os.path.join(repo_root, "tools", "codeowners", "CODEOWNERS.in"),
#                        remove_comment_lines)


# def _read_CODEOWNERS_info_lines(repo_root, remove_comment_lines):
#     return _read_lines(os.path.join(repo_root, "tools/codeowners/CODEOWNERS.info"),
#                        remove_comment_lines)


# def _read_CODEOWNERS_info(repo_root):
#     team_name_to_info = {}
#     for line in _read_CODEOWNERS_info_lines(repo_root, True):
#         team_name, _, info_team_name, _, product_tag = line.split(",")
#         team_name_to_info[team_name.strip()] = (info_team_name.strip(),
#                                                 product_tag.strip())
#     return team_name_to_info



# def _get_final_codeowners(codeowner_lines, team_name_to_info):
#     lines = []
#     for line in codeowner_lines:
#         if line.startswith("#"):
#             lines.append(line)
#         else:
#             #_get_primary_owning_team("")
#             team_name = _get_primary_owning_team(line)
#             if team_name is None:
#                 lines.append(line)
#             else:
#                 team_name, product_tag = team_name_to_info[team_name]
#                 lines.append("#INFO: %s, %s" % (team_name, product_tag))
#                 lines.append(line)
#                 lines.append("") # for readability
#     return "\\n".join(lines)


# def generate(root_path):
#     team_name_to_info = _read_CODEOWNERS_info(root_path)
#     codeowners_in = _read_CODEOWNERS_lines(root_path, False)
#     return  _get_final_codeowners(codeowners_in, team_name_to_info)


# root = "/Users/stoens/Code/root"
# generate(root)
