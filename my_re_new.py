import ply.lex as lex
import ply.yacc as yacc

# tokens
tokens = ("SYMBOL", "OR", "DOT", "PLUS", "OPT", "REPIT", "LRB", "RRB", "ID")


def t_SYMBOL(t):
    r"(&[?+.|&{}<>()])|([^<>?+.|&)({}])"
    sym = str(t.value)
    if len(sym) > 1:
        sym = sym[1]
    t.value = sym
    return t


def t_OR(t):
    r"\|"
    t.value = str(t.value)
    return t


def t_DOT(t):
    r"\."
    t.value = str(t.value)
    return t


def t_PLUS(t):
    r"\+"
    t.value = str(t.value)
    return t


def t_OPT(t):
    r"\?"
    t.value = str(t.value)
    return t


def t_REPIT(t):
    r"\{(([0-9]+)?,([0-9]+)?)?\}"
    bounds = str(t.value)[1:-1]
    if bounds:
        low, top = bounds.split(",")
    else:
        low, top = "", ""
    if not low:
        low = 0
    else:
        low = int(low)
    if not top:
        top = -1
    else:
        top = int(top)
    t.value = (low, top)
    return t


def t_LRB(t):
    r"\("
    t.value = str(t.value)
    return t


def t_RRB(t):
    r"\)"
    t.value = str(t.value)
    return t


# def t_AMP(t):
#     r"&"
#     t.value = str(t.value)
#     return t


def t_ID(t):
    r"\<[A-Za-z][A-Za-z0-9_]*\>"
    t.value = str(t.value)[1:-1]
    return t


def t_error(t):
    global ErrorsList
    ErrorsList.append("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


lexer = lex.lex()
# end of tokens

# define regular expressions
precedence = (("left", "OR"),)


def p_regular(p):
    """regular : reg"""
    p[0] = RootNode(p[1])  # it remembers about (r)# formality


def p_reg(p):
    """reg : orreg
    | concat
    | positclos
    | optional
    | anysym
    | repit
    | ncgroup
    | expncgroup
    | alfsym
    | LRB reg RRB"""
    if len(p) < 3:
        p[0] = p[1]
    else:
        p[0] = p[2]


def p_orreg(p):
    """orreg : reg OR reg"""
    p[0] = OrNode(p[1], p[3])


def p_concat(p):
    """concat : reg reg"""  # ??? reg DOT reg ???
    p[0] = ConcatNode(p[1], p[2])


def p_positclos(p):
    """positclos : reg PLUS"""
    p[0] = PositClosNode(p[1])


def p_optional(p):
    """optional : reg OPT"""
    p[0] = OptNode(p[1])


def p_anysym(p):
    """anysym : DOT"""
    p[0] = SymbolNode()  # char = -1 - special code for symbol


ErrorsList = []


def p_repit(p):
    """repit : reg REPIT"""
    global ErrorsList
    low = p[2][0]
    top = p[2][1]
    if low <= top and top or top < 0:
        if low == 0 and top == -1:
            p[0] = ClosureNode(p[1])
        else:
            p[0] = RepitNode(p[1], low, top)
    elif not top:
        ErrorsList.append("Upper bound must be greater then 0!")
    else:
        ErrorsList.append("Upper bound must not be less then lower one!")


namedCaptureGroups = {}


def p_ncgroup(p):
    """ncgroup : LRB ID reg RRB"""
    global namedCaptureGroups
    if not namedCaptureGroups.get(p[2]):
        p[0] = NCGNode(p[3], p[2])
        namedCaptureGroups[p[2]] = p[3]
    else:
        global ErrorsList
        ErrorsList.append("Redefinition of named capture group:" + p[2])


def p_expncgroup(p):
    """expncgroup : ID"""
    global namedCaptureGroups
    if namedCaptureGroups.get(p[1]):
        p[0] = NCGNode(namedCaptureGroups[p[1]].copy(), p[1])
    else:
        global ErrorsList
        ErrorsList.append("Undefined named capture group: " + p[1])


def p_alfsym(p):
    """alfsym : SYMBOL"""
    p[0] = SymbolNode(p[1])


def p_error(p):
    global ErrorsList
    ErrorsList.append("unexpexted token: " + str(p))


parser = yacc.yacc()
# end of regular expressions

# tree nodes

listNodes = []
followposList = []


class RootNode:  # it remembers about (r)# formality
    def __init__(self, child):
        self.child = child

    def copy(self):
        return RootNode(self.child.copy())

    def nullable(self):
        return self.child.nullable()

    def firstpos(self):
        return self.child.firstpos()

    def lastpos(self):
        return self.child.lastpos()

    def followpos(self):
        global followposList
        for pos in self.child.lastpos():
            followposList[pos].add(-1)
        self.child.followpos()


class OrNode:
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def copy(self):
        return OrNode(self.left.copy(), self.right.copy())

    def nullable(self):
        return self.left.nullable() or self.right.nullable()

    def firstpos(self):
        return self.left.firstpos().union(self.right.firstpos())

    def lastpos(self):
        return self.left.lastpos().union(self.right.lastpos())

    def followpos(self):
        self.left.followpos()
        self.right.followpos()


class ConcatNode:
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def copy(self):
        return ConcatNode(self.left.copy(), self.right.copy())

    def nullable(self):
        return self.left.nullable() and self.right.nullable()

    def firstpos(self):
        return (
            self.left.firstpos().union(self.right.firstpos())
            if self.left.nullable()
            else self.left.firstpos()
        )

    def lastpos(self):
        return (
            self.left.lastpos().union(self.right.lastpos())
            if self.right.nullable()
            else self.right.lastpos()
        )

    def followpos(self):
        global followposList
        firstpos = self.right.firstpos()
        for pos in self.left.lastpos():
            followposList[pos].update(firstpos)
        self.left.followpos()
        self.right.followpos()


class ClosureNode:
    def __init__(self, child):
        self.child = child

    def copy(self):
        return ClosureNode(self.child.copy())

    def nullable(self):
        return True

    def firstpos(self):
        return self.child.firstpos()

    def lastpos(self):
        return self.child.lastpos()

    def followpos(self):
        global followposList
        firstpos = self.child.firstpos()
        for pos in self.child.lastpos():
            followposList[pos].update(firstpos)
        self.child.followpos()


class PositClosNode:
    def __init__(self, child):
        self.child = ConcatNode(child.copy(), ClosureNode(child))

    def copy(self):
        return self.child.copy()

    def nullable(self):
        return self.child.nullable()

    def firstpos(self):
        return self.child.firstpos()

    def lastpos(self):
        return self.child.lastpos()

    def followpos(self):
        self.child.followpos()


class OptNode:
    def __init__(self, child):
        self.child = OrNode(EmptyNode(), child)

    def copy(self):
        return self.child.copy()

    def nullable(self):
        return self.child.nullable()

    def firstpos(self):
        return self.child.firstpos()

    def lastpos(self):
        return self.child.lastpos()

    def followpos(self):
        self.child.followpos()


# class AnySymNode:
#         def __init__(self):
#                 self.char = -1 # special code for any symbol node

#         def nullable(self):
#                 return False


class RepitNode:
    def __init__(self, child, lowerBound=0, upperBound=-1):
        #        self.child = child
        #        self.lowerBound = lowerBound
        #        self.upperBound = upperBound  # -1 means infinity
        if upperBound < 0 and not lowerBound:
            self.child = ClosureNode(child)
        elif upperBound < 0:
            last = ClosureNode(child)
            first = child.copy()
            for i in range(1, lowerBound):
                first = ConcatNode(first, child.copy())
            self.child = ConcatNode(first, last)
        else:
            if not lowerBound:
                first = EmptyNode()
                first = OrNode(first, child)
            else:
                first = child
                for i in range(1, lowerBound):
                    first = ConcatNode(first, child.copy())
            for i in range(lowerBound, upperBound):
                last = ConcatNode(child.copy(), child.copy())
                for j in range(1, i):
                    last = ConcatNode(last, child.copy())
                first = OrNode(first, last)
            self.child = first

    def copy(self):
        return self.child.copy()

    def nullable(self):
        return self.child.nullable()

    def firstpos(self):
        return self.child.firstpos()

    def lastpos(self):
        return self.child.lastpos()

    def followpos(self):
        self.child.followpos()


class NCGNode:
    def __init__(self, child, name):
        self.child = child
        self.name = name

    def copy(self):
        return NCGNode(self.child.copy(), self.name)

    def nullable(self):
        return self.child.nullable()

    def firstpos(self):
        return self.child.firstpos()

    def lastpos(self):
        return self.child.lastpos()

    def followpos(self):
        self.child.followpos()


class SymbolNode:
    def __init__(self, char=-1):
        self.char = char  # -1 - special code for any symbol node
        global listNodes
        global followposList
        self.number = len(listNodes)
        listNodes.append(self)
        followposList.append(set())

    def copy(self):
        return SymbolNode(self.char)

    def nullable(self):
        return False

    def firstpos(self):
        return {
            self.number,
        }  # set

    def lastpos(self):
        return {
            self.number,
        }  # set

    def followpos(self):
        pass  # nothing to do here, that's the job for upper levels


class EmptyNode:
    def __init__(self):
        pass

    def copy(self):
        return EmptyNode()

    def nullable(self):
        return True

    def firstpos(self):
        return set()

    def lastpos(self):
        return set()

    def followpos(self):
        pass


# end of tree nodes


def buildDKA(root):
    root.followpos()
    global listNodes
    global followposList
    unmarkedStateList = [
        tuple(root.firstpos()),
    ]
    StateList = list()
    nextState = {}
    while len(unmarkedStateList):
        R = unmarkedStateList[0]
        if not nextState.get(R):
            nextState[R] = {}
        unmarkedStateList = unmarkedStateList[1:]
        StateList.append(R)
        symbols = set()
        for number in R:
            symbols.add(listNodes[number].char)
        for a in symbols:
            S = set()
            for p in R:
                if p != -1 and listNodes[p].char == a:
                    S.update(followposList[p])
            S = tuple(S)
            if S:
                if S not in unmarkedStateList and S not in StateList:
                    unmarkedStateList.append(S)
                nextState[R][a] = S
    finishStateList = []
    for state in StateList:
        if -1 in state:
            finishStateList.append(state)
    return StateList, nextState, finishStateList


def predict(currString, currState, stateList, nextState, finishStateList):
    result = ""
    string = currString[:]
    state = currState
    while string:
        symbol = string[0]
        string = string[1:]
        if nextState[state].get(symbol):
            state = nextState[state][symbol]
            result = result + symbol
        elif nextState[state].get(-1):  # any symbol
            state = nextState[state][-1]
            result = result + symbol
        else:
            result = ""
            return False  # nothing found
        if currState in finishStateList:
            return True  # good prediction
    return False  # bad prediction


def findnext(string, stateList, nextState, finishStateList):
    result = ""
    currState = stateList[0]
    currString = string[:]
    while currString:
        symbol = currString[0]
        currString = currString[1:]
        if nextState[currState].get(symbol):
            currState = nextState[currState][symbol]
            result = result + symbol
        elif nextState[currState].get(-1):  # any symbol
            currState = nextState[currState][-1]
            result = result + symbol
        else:
            result = ""
            currString = string
            break  # nothing found
        if currState in finishStateList and not predict(
            currString, currState, stateList, nextState, finishStateList
        ):
            break  # result ready
    else:
        currString = string  # all string checked but nothing found
        result = ""
    return result, currString


def initGlobals():
    global listNodes
    global followposList
    global ErrorsList
    global namedCaptureGroups
    listNodes = []
    followposList = []
    ErrorsList = []
    namedCaptureGroups = {}

def findall(regex, string):
    result = []
    initGlobals()
    root = parser.parse(regex)  # build tree
    global ErrorsList
    if not ErrorsList:
        stateList, nextState, finishStateList = buildDKA(root)
        currString = string[:]
        if stateList[0] in finishStateList:
            result.append("")
        while currString:
            newresult, currString = findnext(
                currString, stateList, nextState, finishStateList
            )
            if newresult:
                result.append(newresult)
            else:
                currString = currString[1:]
    return result, ErrorsList


def compile(regex):
    initGlobals()
    root = parser.parse(regex)  # build tree
    global ErrorsList
    global namedCaptureGroups
    if not ErrorsList:
        stateList, nextState, finishStateList = buildDKA(root)
        return DKA(stateList, nextState, finishStateList, namedCaptureGroups.copy(), ErrorsList.copy())
    return DKA(ErrorsList=ErrorsList.copy())


class DKA:
    def __init__(self, stateList=[], nextState=[], finishStateList=[], namedCaptureGroups = {}, ErrorsList=[]):
        self.stateList = stateList
        self.nextState = nextState
        self.finishStateList = finishStateList
        self.ErrorsList = ErrorsList
        self.namedCaptureGroups = namedCaptureGroups
        self.isOk = not ErrorsList

    def findall(self, string):
        result = []
        if self.isOk:
            currString = string[:]
            if self.stateList[0] in self.finishStateList:
                result.append("")
            while currString:
                newresult, currString = findnext(
                    currString, self.stateList, self.nextState, self.finishStateList
                )
                if newresult:
                    result.append(newresult)
                else:
                    currString = currString[1:]
        return result