#include <algorithm>
#include <fstream>
#include <iostream>
#include <numeric>
#include <optional>
#include <sstream>
#include <string>
#include <variant>
#include <vector>

enum entry_t { file, dir };

struct Entry {
  entry_t typ;
  int size; // for dir total size of children
  std::string name;
  std::vector<Entry> entries;
};

void print_entry(const int n, const Entry &e) {
  std::cout << std::string(n, ' ');
  switch (e.typ) {
  case file:
    std::cout << "- " << e.name << " (file, size = " << e.size << ")"
              << std::endl;
    break;
  case dir:
    std::cout << "- " << e.name << " (dir, size = " << e.size << ")"
              << std::endl;
    for (const Entry &child : e.entries) {
      print_entry(n + 2, child);
    }
    break;
  default:
    std::cout << "Wat " << std::endl;
  }
}

std::vector<Entry> parseEntries(std::ifstream &input) {
  std::vector<Entry> entries;
  std::string line;
  while (input.peek() != '$' && std::getline(input, line)) {
    if (line.find("dir ") == 0) {
      std::string name = line.substr(4);
      entries.push_back(Entry{dir, 0, name, std::vector<Entry>()});
    } else {
      int size;
      std::string name;
      std::istringstream iss(line);
      iss >> size >> name;
      entries.push_back(Entry{file, size, name, std::vector<Entry>()});
    }
  }
  return entries;
}

Entry parse(std::ifstream &input) {
  Entry entry{.size = 0};
  std::string line;
  while (std::getline(input, line)) {
    if (line == "$ cd /") {
      entry.name = "/";
      entry.typ = dir;
    } else if (line == "$ cd ..") {
      break;
    } else if (line.find("$ cd ") == 0) {
      std::string name = line.substr(5);
      std::optional<Entry> child = parse(input);
      if (child) {
        child->name = name;
        child->typ = dir;
        auto it =
            std::find_if(entry.entries.begin(), entry.entries.end(),
                         [&name](const Entry &e) { return e.name == name; });
        if (it != entry.entries.end()) {
          *it = *child;
        }
      }
    } else if (line == "$ ls") {
      entry.entries = parseEntries(input);
    }
  }
  for (const Entry &child : entry.entries) {
    entry.size += child.size;
  }
  return entry;
}

long total_sum(const Entry &e) {
  switch (e.typ) {
  case file:
    return 0;
  case dir:
    long sum = e.size > 100000 ? 0 : e.size;
    for (const Entry &child : e.entries) {
      sum += total_sum(child);
    }
    return sum;
  }
}

const long FULL = 70000000;
const long NEED = 30000000;

std::vector<Entry> candidates(const Entry &e, long used) {
  std::vector<Entry> res;
  if (e.typ == dir) {
    long left = used + e.size;
    if (FULL - used >= NEED) {
      res.push_back(e);
      for (const Entry &child : e.entries) {
        auto cands = candidates(child, left - child.size);
        res.insert(res.end(), cands.begin(), cands.end());
      }
    }
  }
  return res;
}

int part2(const Entry &root) {
  auto cands = candidates(root, 0);
  auto min = std::min_element(
      cands.begin(), cands.end(),
      [](const auto &e1, const auto &e2) { return e1.size < e2.size; });
  return min->size;
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    std::cerr << "Usage: " << argv[0] << " <file_name>" << std::endl;
    return 1;
  }

  std::string file_name = argv[1];

  std::ifstream input_file(file_name);
  Entry root = parse(input_file);

  // std::cout << "Parsing done" << std::endl;
  // print_entry(0, root);

  long part1 = total_sum(root);
  std::cout << "Part 1 answer: " << part1 << std::endl;

  std::cout << "Part 2 answer: " << part2(root) << std::endl;

  return 0;
}
