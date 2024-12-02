//
// Created by Jared Summers on 12/1/24.
//

#ifndef SOLVER_H
#define SOLVER_H

#include <string>
#include <map>
#include <iostream>
#include <utility>

class solver {
public:
    virtual ~solver() = default;
    virtual void part1();
    virtual void part2();
    explicit solver(std::string filename): source(std::move(filename)) {}
private:
    std::string source;
};

/**
 * Where to register solvers.
 */
class registry {
public:
    bool register_solver(int id, const solver& s) {
        solvers.insert(std::pair(id, s));
        return true;
    };
    void run() {
        for (auto [k, s] : solvers) {
            s.part1();
            s.part2();
        }
    }
protected:
    std::map<int, solver> solvers;
};

extern registry solver_registry;

#endif //SOLVER_H
