#include <cassert>
#include <cmath>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

std::vector<unsigned long long> ParseInput(const std::string &s) {
  std::string token = "";
  std::vector<unsigned long long> ret;
  for (char c : s) {
    if (c == ' ') {
      int num = std::stoi(token);
      ret.push_back(num);
      token = "";
    } else {
      token.push_back(c);
    }
  }
  int num = std::stoi(token);
  ret.push_back(num);
  return ret;
}

int solution1(std::string &input, int numBlinks) {
  std::vector stones = ParseInput(input);
  for (int j = 0; j < numBlinks; j++) {
    for (int j = 0; j < stones.size(); j++) {
      unsigned long long stone = stones[j];
      if (stone == 0)
        stones[j] = 1;
      else {
        std::string stoneStr = std::to_string(stone);
        int n = stoneStr.length();
        if (n % 2 == 0) {
          long divisor = std::pow(10, (n / 2));
          unsigned long long leftStone = stone / divisor;
          unsigned long long rightStone = stone % divisor;
          stones[j] = leftStone;
          stones.insert(stones.begin() + j + 1, rightStone);
          j++;
        } else {
          stones[j] = stone * 2024;
        }
      }
    }
  }
  return stones.size();
}

int main(int argc, char **argv) {
  std::string inputTest = "125 17";
  std::string input;
  std::fstream newfile;
  newfile.open("input.txt", std::ios::in);
  std::getline(newfile, input);

  std::cout << "Part 1" << std::endl;
  assert(solution1(inputTest, 25) == 55312);
  std::cout << "Test passed" << std::endl;
  std::cout << "Solution" << std::endl;
  std::cout << solution1(input, 25) << std::endl;

  std::cout << "Part 2" << std::endl;
  std::cout << solution1(input, 75) << std::endl;
}
