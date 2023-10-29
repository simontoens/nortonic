def get_artifact_and_version(gav):
    i = gav.find(":")
    return i
art_id = get_artifact_and_version("g1:a1:v")
print(art_id)
