from tests import compilertest
import unittest


class MiscTest(compilertest.CompilerTest):

    def setUp(self):
        self.maxDiff = None

    def test_return_result_of_if_expr(self):
        py = """
def get_artifact_and_version(gav):
    i = gav.find(":")
    return None if i == -1 else gav[i + 1:].split()[0]
art_id = get_artifact_and_version("g1:a1:v")
print(art_id)
"""
        self.py(py, expected=py)

        self.java(py, expected="""
static String get_artifact_and_version(String gav) {
    Integer i = gav.indexOf(":");
    return i == -1 ? null : Arrays.asList(gav.substring(i + 1).split(" ")).get(0);
}
static String art_id = get_artifact_and_version("g1:a1:v");
System.out.println(art_id);
""")

        self.elisp(py, expected="""
(defun get_artifact_and_version (gav)
    (setq i (cl-search ":" gav))
    (if (equal i nil)
        (setq i -1))
    (if (equal i -1)
        nil
        (nth 0 (split-string (substring gav (+ i 1))))))
(setq art_id (get_artifact_and_version "g1:a1:v"))
(message art_id)
""")

        self.go(py, expected="""
func get_artifact_and_version(gav *string) *string {
    i := strings.Index(*gav, ":")
    if i == -1 {
        return nil
    } else {
        t := strings.Split((*gav)[i + 1:] , " ")[0]
        return &t
    }
}
t1 := "g1:a1:v"
art_id := get_artifact_and_version(&t1)
fmt.Println(*art_id)
""")

    def test_unpack_func_rtn_value(self):
        py = """
def get_age_and_fav_num(birthyear):
    this_year = 2022
    return this_year - birthyear, 4

age, num = get_age_and_fav_num(2015)
print("Age is", age, "and favorite number is", num)
"""

        self.py(py, expected="""
def get_age_and_fav_num(birthyear):
    this_year = 2022
    return this_year - birthyear, 4
age, num = get_age_and_fav_num(2015)
print("Age is", age, "and favorite number is", num)
""")

        self.java(py, expected="""
static Tuple<Integer, Integer> get_age_and_fav_num(Integer birthyear) {
    Integer this_year = 2022;
    return Tuple.of(this_year - birthyear, 4);
}
static Tuple<Integer, Integer> t = get_age_and_fav_num(2015);
static Integer age = t.get(0);
static Integer num = t.get(1);
System.out.println(String.format("%s %d %s %d", "Age is", age, "and favorite number is", num));
""")

        self.elisp(py, expected="""
(defun get_age_and_fav_num (birthyear)
    (setq this_year 2022)
    (list (- this_year birthyear) 4))
(setq t (get_age_and_fav_num 2015))
(setq age (nth 0 t))
(setq num (nth 1 t))
(message "%s %s %s %s" "Age is" age "and favorite number is" num)
""")

        self.go(py, expected="""
func get_age_and_fav_num(birthyear int) (int, int) {
    this_year := 2022
    return this_year - birthyear, 4
}
age, num := get_age_and_fav_num(2015)
fmt.Println("Age is", age, "and favorite number is", num)
""")

    def test_non_trivial1(self):
        py = """
def get_counter_info(initial_value, increment):
    print("initial value is", initial_value)
    return 0, 1
def get_middle_element(names):
    c_info = get_counter_info(0, 1)
    counter = c_info[0]
    middle_element_index = None
    for name in names:
        counter = counter + c_info[1]
        last_element = name
        if counter == len(names) / 2:
            middle_element_index = counter
    print("last element processed:", last_element)
    return names[middle_element_index]
el = get_middle_element(("e1", "e2", "e3", "e4"))
print("the element closest to the middle is", el)
"""

        self.py(py, expected=py)

        self.java(py, expected="""
static Tuple<Integer, Integer> get_counter_info(Integer initial_value, Integer increment) {
    System.out.println(String.format("%s %d", "initial value is", initial_value));
    return Tuple.of(0, 1);
}
static String get_middle_element(Tuple<String, String, String, String> names) {
    Tuple<Integer, Integer> c_info = get_counter_info(0, 1);
    Integer counter = c_info.get(0);
    Integer middle_element_index = null;
    String last_element = null;
    for (String name : names) {
        counter = counter + c_info.get(1);
        last_element = name;
        if (counter == names.size() / 2) {
            middle_element_index = counter;
        }
    }
    System.out.println(String.format("%s %s", "last element processed:", last_element));
    return names.get(middle_element_index);
}
static String el = get_middle_element(Tuple.of("e1", "e2", "e3", "e4"));
System.out.println(String.format("%s %s", "the element closest to the middle is", el));
""")

        self.elisp(py, expected="""
(defun get_counter_info (initial_value increment)
    (message "%s %s" "initial value is" initial_value)
    (list 0 1))
(defun get_middle_element (names)
    (setq c_info (get_counter_info 0 1))
    (setq counter (nth 0 c_info))
    (setq middle_element_index nil)
    (dolist (name names)
        (setq counter (+ counter (nth 1 c_info)))
        (setq last_element name)
        (if (equal counter (/ (length names) 2))
            (setq middle_element_index counter)))
    (message "%s %s" "last element processed:" last_element)
    (nth middle_element_index names))
(setq el (get_middle_element (list "e1" "e2" "e3" "e4")))
(message "%s %s" "the element closest to the middle is" el)
""")

        self.go(py, expected="""
func get_counter_info(initial_value, increment int) []int {
    fmt.Println("initial value is", initial_value)
    return []int{0, 1}
}
func get_middle_element(names []string) *string {
    c_info := get_counter_info(0, 1)
    counter := c_info[0]
    var middle_element_index int
    var last_element string
    for i := 0; i < len(names); i += 1 {
        name := names[i]
        counter = counter + c_info[1]
        last_element = name
        if counter == len(names) / 2 {
            middle_element_index = counter
        }
    }
    fmt.Println("last element processed:", last_element)
    t := names[middle_element_index]
    return &t
}
el := get_middle_element([]string{"e1", "e2", "e3", "e4"})
fmt.Println("the element closest to the middle is", *el)
""")

    def test_codeowners(self):
        py = """
import os


def _read_lines(path, remove_comment_lines):
    lines = []
    with open(path) as f:
        for line in f.readlines():
            line = line.strip()
            skip_line = False
            if remove_comment_lines and line.startswith("#"):
                skip_line = True
            if not skip_line:
                lines.append(line)
    return lines


def _get_primary_owning_team(codeowners_line):
    TEAM_PREFIX = "@org1/"
    i = codeowners_line.find(TEAM_PREFIX)
    return None if i == -1 else codeowners_line[i:].split()[0]


def _read_CODEOWNERS_lines(repo_root, remove_comment_lines):
    return _read_lines(os.path.join(repo_root, "tools", "codeowners", "CODEOWNERS.in"),
                       remove_comment_lines)


def _read_CODEOWNERS_info_lines(repo_root, remove_comment_lines):
    return _read_lines(os.path.join(repo_root, "tools/codeowners/CODEOWNERS.info"),
                       remove_comment_lines)


def _read_CODEOWNERS_info(repo_root):
    team_name_to_info = {}
    for line in _read_CODEOWNERS_info_lines(repo_root, True):
        team_name, _, info_team_name, _, product_tag = line.split(",")
        team_name_to_info[team_name.strip()] = (info_team_name.strip(),
                                                product_tag.strip())
    return team_name_to_info



def _get_final_codeowners(codeowner_lines, team_name_to_info):
    lines = []
    for line in codeowner_lines:
        if line.startswith("#"):
            lines.append(line)
        else:
            team_name = _get_primary_owning_team(line)
            if team_name is None:
                lines.append(line)
            else:
                team_name, product_tag = team_name_to_info[team_name]
                lines.append("#INFO: %s, %s" % (team_name, product_tag))
                lines.append(line)
                lines.append("") # for readability
    return "\\n".join(lines)


def generate(root_path):
    team_name_to_info = _read_CODEOWNERS_info(root_path)
    codeowners_in = _read_CODEOWNERS_lines(root_path, False)
    return  _get_final_codeowners(codeowners_in, team_name_to_info)


root = "/Users/stoens/Code/root"
updated_codeowners = generate(root)
with open(os.path.join(root, "CODEOWNERS"), "w") as f:
    f.write(updated_codeowners)
"""

        self.py(py, expected="""
def _read_lines(path, remove_comment_lines):
    lines = []
    f = open(path)
    for line in f.readlines():
        line = line.strip()
        skip_line = False
        if remove_comment_lines and line.startswith("#"):
            skip_line = True
        if not skip_line:
            lines.append(line)
    return lines
def _get_primary_owning_team(codeowners_line):
    TEAM_PREFIX = "@org1/"
    i = codeowners_line.find(TEAM_PREFIX)
    return None if i == -1 else codeowners_line[i:].split()[0]
def _read_CODEOWNERS_lines(repo_root, remove_comment_lines):
    return _read_lines(os.path.join(repo_root, "tools", "codeowners", "CODEOWNERS.in"), remove_comment_lines)
def _read_CODEOWNERS_info_lines(repo_root, remove_comment_lines):
    return _read_lines(os.path.join(repo_root, "tools/codeowners/CODEOWNERS.info"), remove_comment_lines)
def _read_CODEOWNERS_info(repo_root):
    team_name_to_info = {}
    for line in _read_CODEOWNERS_info_lines(repo_root, True):
        team_name, _, info_team_name, _, product_tag = line.split(",")
        team_name_to_info[team_name.strip()] = (info_team_name.strip(), product_tag.strip() )
    return team_name_to_info
def _get_final_codeowners(codeowner_lines, team_name_to_info):
    lines = []
    for line in codeowner_lines:
        if line.startswith("#"):
            lines.append(line)
        else:
            team_name = _get_primary_owning_team(line)
            if team_name is None:
                lines.append(line)
            else:
                team_name, product_tag = team_name_to_info[team_name]
                lines.append("#INFO: %s, %s" % (team_name, product_tag))
                lines.append(line)
                lines.append("")
    return "\n".join(lines)
def generate(root_path):
    team_name_to_info = _read_CODEOWNERS_info(root_path)
    codeowners_in = _read_CODEOWNERS_lines(root_path, False)
    return _get_final_codeowners(codeowners_in, team_name_to_info)
root = "/Users/stoens/Code/root"
updated_codeowners = generate(root)
f = open(os.path.join(root, "CODEOWNERS"), "w")
f.write(updated_codeowners)
""")


        self.java(py, expected="""
static List<String> _read_lines(String path, Boolean remove_comment_lines) throws IOException {
    List<String> lines = new ArrayList<>();
    File f = new File(path);
    for (String line : Arrays.asList(Files.readString(f.toPath()).split("\\n"))) {
        line = line.trim();
        Boolean skip_line = false;
        if (remove_comment_lines && line.startsWith("#")) {
            skip_line = true;
        }
        if (!skip_line) {
            lines.add(line);
        }
    }
    return lines;
}
static String _get_primary_owning_team(String codeowners_line) {
    String TEAM_PREFIX = "@org1/";
    Integer i = codeowners_line.indexOf(TEAM_PREFIX);
    return i == -1 ? null : Arrays.asList(codeowners_line.substring(i).split(" ")).get(0);
}
static List<String> _read_CODEOWNERS_lines(String repo_root, Boolean remove_comment_lines) throws IOException {
    return _read_lines(String.valueOf(Paths.get(repo_root, "tools", "codeowners", "CODEOWNERS.in")), remove_comment_lines);
}
static List<String> _read_CODEOWNERS_info_lines(String repo_root, Boolean remove_comment_lines) throws IOException {
    return _read_lines(String.valueOf(Paths.get(repo_root, "tools/codeowners/CODEOWNERS.info")), remove_comment_lines);
}
static Map<String, Tuple<String, String>> _read_CODEOWNERS_info(String repo_root) throws IOException {
    Map<String, Tuple<String, String>> team_name_to_info = new HashMap<>(Map.of());
    for (String line : _read_CODEOWNERS_info_lines(repo_root, true)) {
        List<String> t = Arrays.asList(line.split(","));
        String team_name = t.get(0);
        String info_team_name = t.get(2);
        String product_tag = t.get(4);
        team_name_to_info.put(team_name.trim(), Tuple.of(info_team_name.trim(), product_tag.trim() ));
    }
    return team_name_to_info;
}
static String _get_final_codeowners(List<String> codeowner_lines, Map<String, Tuple<String, String>> team_name_to_info) {
    List<String> lines = new ArrayList<>();
    for (String line : codeowner_lines) {
        if (line.startsWith("#")) {
            lines.add(line);
        } else {
            String team_name = _get_primary_owning_team(line);
            if (team_name == null) {
                lines.add(line);
            } else {
                Tuple<String, String> t1 = team_name_to_info.get(team_name);
                team_name = t1.get(0);
                String product_tag = t1.get(1);
                lines.add(String.format("#INFO: %s, %s", team_name, product_tag));
                lines.add(line);
                lines.add("");
            }
        }
    }
    return String.join("\n", lines);
}
static String generate(String root_path) throws IOException {
    Map<String, Tuple<String, String>> team_name_to_info = _read_CODEOWNERS_info(root_path);
    List<String> codeowners_in = _read_CODEOWNERS_lines(root_path, false);
    return _get_final_codeowners(codeowners_in, team_name_to_info);
}
static String root = "/Users/stoens/Code/root";
static String updated_codeowners = generate(root);
static File f = new File(String.valueOf(Paths.get(root, "CODEOWNERS")));
Files.writeString(f.toPath(), updated_codeowners, Charset.defaultCharset());
""")        


        self.go(py, expected="""
func _read_lines(path *string, remove_comment_lines bool) *[]string {
    lines := []string{}
    f, _ := os.Open(*path)
    t8, _ := os.ReadFile(f.Name())
    t2 := strings.Split(string(t8), "\\n")
    for i1 := 0; i1 < len(t2); i1 += 1 {
        line := t2[i1]
        line = strings.TrimSpace(line)
        skip_line := false
        if remove_comment_lines && strings.HasPrefix(line, "#" ) {
            skip_line = true
        }
        if !skip_line {
            lines = append(lines, line)
        }
    }
    return &lines
}
func _get_primary_owning_team(codeowners_line *string) *string {
    TEAM_PREFIX := "@org1/"
    i := strings.Index(*codeowners_line, TEAM_PREFIX)
    if i == -1 {
        return nil
    } else {
        t4 := strings.Split((*codeowners_line)[i:] , " ")[0]
        return &t4
    }
}
func _read_CODEOWNERS_lines(repo_root *string, remove_comment_lines bool) *[]string {
    t5 := filepath.Join(*repo_root, "tools", "codeowners", "CODEOWNERS.in")
    return _read_lines(&t5, remove_comment_lines)
}
func _read_CODEOWNERS_info_lines(repo_root *string, remove_comment_lines bool) *[]string {
    t6 := filepath.Join(*repo_root, "tools/codeowners/CODEOWNERS.info")
    return _read_lines(&t6, remove_comment_lines)
}
func _read_CODEOWNERS_info(repo_root *string) map[string][]string {
    team_name_to_info := map[string][]string{}
    t3 := _read_CODEOWNERS_info_lines(repo_root, true)
    for i2 := 0; i2 < len(*t3); i2 += 1 {
        line := (*t3)[i2]
        t := strings.Split(line, ",")
        team_name := t[0]
        info_team_name := t[2]
        product_tag := t[4]
        team_name_to_info[strings.TrimSpace(team_name)] = []string{strings.TrimSpace(info_team_name), strings.TrimSpace(product_tag) }
    }
    return team_name_to_info
}
func _get_final_codeowners(codeowner_lines *[]string, team_name_to_info map[string][]string) *string {
    lines := []string{}
    for i3 := 0; i3 < len(*codeowner_lines); i3 += 1 {
        line := (*codeowner_lines)[i3]
        if strings.HasPrefix(line, "#" ) {
            lines = append(lines, line)
        } else {
            team_name := _get_primary_owning_team(&line)
            if team_name == nil {
                lines = append(lines, line)
            } else {
                t1 := team_name_to_info[*team_name]
                team_name = &t1[0]
                product_tag := t1[1]
                lines = append(lines, fmt.Sprintf("#INFO: %s, %s", *team_name, product_tag))
                lines = append(lines, line)
                lines = append(lines, "")
            }
        }
    }
    t7 := strings.Join(lines, "\n")
    return &t7
}
func generate(root_path *string) *string {
    team_name_to_info := _read_CODEOWNERS_info(root_path)
    codeowners_in := _read_CODEOWNERS_lines(root_path, false)
    return _get_final_codeowners(codeowners_in, team_name_to_info)
}
root := "/Users/stoens/Code/root"
updated_codeowners := generate(&root)
f, _ := os.Create(filepath.Join(root, "CODEOWNERS"))
_ = os.WriteFile(f.Name(), []byte(*updated_codeowners), 0644)	
""")

        self.elisp(py, expected="""
(defun _read_lines (path remove_comment_lines)
    (setq lines (list))
    (setq f path)
    (dolist (line (split-string
        (with-temp-buffer
            (insert-file-contents f)
            (buffer-string))
        "\\n"))
        (setq line (string-trim line))
        (setq skip_line nil)
        (if (and remove_comment_lines (string-prefix-p "#" line))
            (setq skip_line t))
        (if (not skip_line)
            (add-to-list 'lines line)))
    lines)
(defun _get_primary_owning_team (codeowners_line)
    (setq TEAM_PREFIX "@org1/")
    (setq i (cl-search TEAM_PREFIX codeowners_line))
    (if (equal i nil)
        (setq i -1))
    (if (equal i -1)
        nil
        (nth 0 (split-string (substring codeowners_line i)))))
(defun _read_CODEOWNERS_lines (repo_root remove_comment_lines)
    (_read_lines (f-join repo_root "tools" "codeowners" "CODEOWNERS.in") remove_comment_lines))
(defun _read_CODEOWNERS_info_lines (repo_root remove_comment_lines)
    (_read_lines (f-join repo_root "tools/codeowners/CODEOWNERS.info") remove_comment_lines))
(defun _read_CODEOWNERS_info (repo_root)
    (setq team_name_to_info #s(hash-table test equal data ()))
    (dolist (line (_read_CODEOWNERS_info_lines repo_root t))
        (setq t (split-string line ","))
        (setq team_name (nth 0 t))
        (setq info_team_name (nth 2 t))
        (setq product_tag (nth 4 t))
        (puthash (string-trim team_name) (list (string-trim info_team_name) (string-trim product_tag)) team_name_to_info))
    team_name_to_info)
(defun _get_final_codeowners (codeowner_lines team_name_to_info)
    (setq lines (list))
    (dolist (line codeowner_lines)
        (if (string-prefix-p "#" line)
            (add-to-list 'lines line)
            (setq team_name (_get_primary_owning_team line))
            (if (eq team_name nil)
                (add-to-list 'lines line)
                (setq t1 (gethash team_name team_name_to_info))
                (setq team_name (nth 0 t1))
                (setq product_tag (nth 1 t1))
                (add-to-list 'lines (format "#INFO: %s, %s" team_name product_tag))
                (add-to-list 'lines line)
                (add-to-list 'lines ""))))
    (mapconcat 'identity lines "\n"))
(defun generate (root_path)
    (setq team_name_to_info (_read_CODEOWNERS_info root_path))
    (setq codeowners_in (_read_CODEOWNERS_lines root_path nil))
    (_get_final_codeowners codeowners_in team_name_to_info))
(setq root "/Users/stoens/Code/root")
(setq updated_codeowners (generate root))
(setq f (f-join root "CODEOWNERS"))
(with-temp-file f
    (insert updated_codeowners))
""")        


if __name__ == '__main__':
    unittest.main()
