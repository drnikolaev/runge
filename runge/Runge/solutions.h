#ifndef _RUNGE_SOLUTIONS_
#define _RUNGE_SOLUTIONS_

#include <stdexcept>
#include <iostream>

#include "../solver.h"
#include "lock.h"

#if defined(WIN32) && defined(_DEBUG)
     #define DEBUG_NEW new( _NORMAL_BLOCK, __FILE__, __LINE__ )
     #define new DEBUG_NEW
#endif

class SolutionPoint {
    double t;
    cvm::rvector x;
    double h;

public:
	SolutionPoint(int dimension)
		: t(0.), x(dimension), h(0.) {
	}
    SolutionPoint(double _t, const cvm::rvector& _x, double _h)
        : t(_t), x(_x), h(_h) {
    }
    SolutionPoint(const SolutionPoint& that)
        : t(that.t), x(that.x), h(that.h) {
    }
    double get_t() const {
        return t;
    }
    double get_h() const {
        return h;
    }
    const cvm::rvector& get_x() const {
        return x;
    }
	double get_value(int index, bool dependent) const {
		return dependent ? get_x()(index) : get_t();
	}

    friend std::ostream& operator << (std::ostream& os, const SolutionPoint& point);
    friend std::istream& operator >> (std::istream& is, SolutionPoint& point);
};

class Solution {
	int dimension;
    double t0;
    double eps; 
    double p;
    std::vector<std::string> var_names;
    std::vector<std::string> equations;
    std::vector<std::string> macros;
    std::vector<std::string> macros_meanings;
    std::vector<SolutionPoint> pts;
	long long start_time_us;
	double time_ms;
    std::vector<double> min_vals, max_vals;
    bool selected;
	unsigned int r_color, g_color, b_color, a_color;
	unsigned int r_color2, g_color2, b_color2, a_color2;

public:
	static QMutex& mutex() {
		static QMutex _mutex;
		return _mutex;
	}
    Solution() :
		dimension(0), t0(0.), eps(0.), p(0.), start_time_us(0LL), time_ms(0.), selected(false),
		r_color(0x0), g_color(0x0), b_color(0x0), a_color(0xFF),
		r_color2(0x0), g_color2(0x0), b_color2(0x0), a_color2(0xFF){
    }
    Solution(int _dimension, double _t0, double _eps, double _p, 
		const std::vector<std::string>& _var_names, const std::vector<std::string>& _equations,
        const std::vector<std::string>& _macros, const std::vector<std::string>& _macros_meanings,
        long long _start_time_us, 
		int _r_color, int _g_color, int _b_color, int _a_color,
		int _r_color2, int _g_color2, int _b_color2, int _a_color2) :
			dimension(_dimension), t0(_t0), eps(_eps), p(_p), 
			var_names(_var_names), equations(_equations), 
            macros(_macros), macros_meanings(_macros_meanings),
            start_time_us(_start_time_us), time_ms(0.),
			min_vals(dimension+2), max_vals(dimension+2), selected(false),
			r_color(_r_color), g_color(_g_color), b_color(_b_color), a_color(_a_color),
			r_color2(_r_color2), g_color2(_g_color2), b_color2(_b_color2), a_color2(_a_color2) {
    }
	~Solution () {
	}
/*
    void set_properties(int _dimension, double _t0, double _eps, double _p, 
		const std::vector<std::string>& _var_names, long long _start_time_us)
	{
        dimension = _dimension;
		t0 = _t0;
		eps = _eps;
		p = _p; 
		var_names = _var_names;
		start_time_us = _start_time_us;
		time_ms = 0.;
		min_vals.resize(dimension+2);
		max_vals.resize(dimension+2);
		selected = false;
    }
*/
    void add_solution_point(double t, const cvm::rvector& x, double h) {
        add_solution_point(SolutionPoint(t, x, h));
    }
    void add_solution_point(const SolutionPoint& pt);

    int get_dimension() const {
        return dimension;
    }
    double get_t0() const {
        return t0;
    }
    double get_eps() const {
        return eps;
    }
    double get_p() const {
        return p;
    }
    const std::vector<std::string>& get_var_names() const {
        return var_names;
    }
    const std::vector<std::string>& get_equations() const {
        return equations;
    }
    const std::vector<std::string>& get_macros() const {
        return macros;
    }
    const std::vector<std::string>& get_macros_meanings() const {
        return macros_meanings;
    }
    const std::vector<SolutionPoint>& get_pts() const {
        return pts;
    }
	void set_end_time(long long end_time_us) {
        time_ms = static_cast<double>(end_time_us - start_time_us) / 1.e3;
	}
	void set_time(double _time_ms) {
        time_ms = _time_ms;
	}
	double get_time_ms() const {
		return time_ms;
	}
	double get_min_val(int var_index) const {
		return pts.size() <= 0 ? 0. : min_vals[var_index];
	}
	double get_max_val(int var_index) const {
		return pts.size() <= 0 ? 0. : max_vals[var_index];
	}
    void set_selected(bool _selected) {
        selected = _selected;
    }
    bool is_selected() const {
        return selected;
    }
	void get_color(bool odd, unsigned int* r, unsigned int* g, unsigned int* b, unsigned int* a) const {
		*r = odd ? r_color : r_color2;
		*g = odd ? g_color : g_color2;
		*b = odd ? b_color : b_color2;
		*a = odd ? a_color : a_color2;
	}
	void set_color(bool odd, unsigned int r, unsigned int g, unsigned int b, unsigned int a) {
		if (odd) {
			r_color = r;
			g_color = g;
			b_color = b;
			a_color = a;
		} else {
			r_color2 = r;
			g_color2 = g;
			b_color2 = b;
			a_color2 = a;
		}
	}

    int get_var_index(const std::string& var) const;
	bool get_start(int index, double& value) const;
	bool get_end(int index, double& value) const;
    void export_to_scv(const std::string& file_name) const throw (std::exception);
    void export_to_matlab(const std::string& file_name, const std::string& var_name) const throw (std::exception);
};

class Solutions : public std::map<int, Solution> {
public:
    typedef std::map<int, Solution>::const_iterator const_iterator;
    typedef std::map<int, Solution>::iterator iterator;

    Solutions() {
	}
    int add_solution(int dimension, double t0, double eps, double p, 
		const std::vector<std::string>& var_names, const std::vector<std::string>& equations,
        const std::vector<std::string>& macros, const std::vector<std::string>& macros_meanings,
        long long start_time_us,
		int r_color, int g_color, int b_color, int a_color,
		int r_color2, int g_color2, int b_color2, int a_color2)
    {
        int id = rbegin() == rend() ? 0 : rbegin()->first + 1;
        insert(std::pair<int, Solution>(id, Solution(dimension, t0, eps, p, var_names, equations,
            macros, macros_meanings, start_time_us, 
			r_color, g_color, b_color, a_color, 
			r_color2, g_color2, b_color2, a_color2)));
        return id;
    }
	void add_solution(int id, const Solution& solution) {
        (*this)[id] = solution;
    }
    void add_solution_point(int id, double t, const cvm::rvector& x, double h) {
        (*this)[id].add_solution_point(t, x, h);
    }
    const Solution& get_solution(int id) const {
        return at(id);
    }
    Solution& get_solution(int id) {
        return at(id);
    }
    void remove_solution(int id) {
        erase(id);
    }
	void set_end_time(int id, long long end_time_us) {
        (*this)[id].set_end_time(end_time_us);
	}
};

#endif // !_RUNGE_SOLUTIONS_
