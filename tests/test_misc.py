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
    (if (equal i -1)
        nil
        (nth 0 (split-string (substring gav (+ i 1))))))
(setq art_id (get_artifact_and_version "g1:a1:v"))
(message art_id)
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
static List<Integer> get_age_and_fav_num(Integer birthyear) {
    Integer this_year = 2022;
    return List.of(this_year - birthyear, 4);
}
static List<Integer> t0 = get_age_and_fav_num(2015);
static Integer age = t0.get(0);
static Integer num = t0.get(1);
System.out.println(String.format("%s %d %s %d", "Age is", age, "and favorite number is", num));
""")

        self.elisp(py, expected="""
(defun get_age_and_fav_num (birthyear)
    (setq this_year 2022)
    (list (- this_year birthyear) 4))
(setq t0 (get_age_and_fav_num 2015))
(setq age (nth 0 t0))
(setq num (nth 1 t0))
(message "%s %s %s %s" "Age is" age "and favorite number is" num)
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
static List<Integer> get_counter_info(Integer initial_value, Integer increment) {
    System.out.println(String.format("%s %d", "initial value is", initial_value));
    return List.of(0, 1);
}
static String get_middle_element(List<String> names) {
    String last_element = null;
    List<Integer> c_info = get_counter_info(0, 1);
    Integer counter = c_info.get(0);
    Integer middle_element_index = null;
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
static String el = get_middle_element(List.of("e1", "e2", "e3", "e4"));
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
    

if __name__ == '__main__':
    unittest.main()
