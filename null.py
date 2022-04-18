class Null:
    """
    A None placeholder, to distinguish, where it matters, between Python's
    None, which could be a bug or at least indicate a problem, when a lookup
    returns None for example, and an "intentional None" that needs to get
    translated into the target language.
    """
    pass


value = Null()    
