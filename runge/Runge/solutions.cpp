#include <fstream>

#include "solutions.h"
#include "lock.h"

std::ostream& operator << (std::ostream& os, const SolutionPoint& point)
{
    static const std::string space = " ";
#if !defined (CVM_USES_STLPORT) && defined (_MSC_VER)
    os.imbue (std::locale::empty());
#endif
    os << point.t << space << point.x;
    os.seekp(-2, std::ios::cur);	// to remove EOL and one extra space
    os << space << point.h;
    return os;
}

std::istream& operator >> (std::istream& is, SolutionPoint& point)
{
#if !defined (CVM_USES_STLPORT) && defined (_MSC_VER)
    is.imbue (std::locale::empty());
#endif
    is >> point.t >> point.x >> point.h;
    return is;
}

int Solution::get_var_index(const std::string& var) const
{
    int i = -1, index = -1;
    std::vector<std::string>::const_iterator it = var_names.begin();
    while (it != var_names.end()) {
        ++i;
        if (var == *it++) {
            index = i;
            break;
        }
    }
    return index;
}

bool Solution::get_start(int index, double& value) const
{
	bool ret = false;
    std::vector<SolutionPoint>::const_iterator ip = pts.begin();
    if (ip != pts.end()) {
		if (index >= 0 && index <= dimension + 1) {
			value = index == 0 ? ip -> get_t() : (index <= dimension ? ip -> get_x()(index) : ip->get_h());
			ret = true;
		}
	}
	return ret;
}

bool Solution::get_end(int index, double& value) const
{
	bool ret = false;
    std::vector<SolutionPoint>::const_reverse_iterator ip = pts.rbegin();
    if (ip != pts.rend()) {
		if (index >= 0 && index <= dimension + 1) {
			value = index == 0 ? ip -> get_t() : (index <= dimension ? ip -> get_x()(index) : ip->get_h());
			ret = true;
		}
	}
	return ret;
}

void Solution::add_solution_point(const SolutionPoint& pt) {
    const int dimension1 = dimension + 1;
    if (pts.size() <= 0) {  // first point
        min_vals[0] = max_vals[0] = pt.get_t();
        for (int i = 1; i <= dimension; ++i) {
            min_vals[i] = max_vals[i] = pt.get_x()(i);
        }
        min_vals[dimension1] = max_vals[dimension1] = pt.get_h();
    } else {
        if (pt.get_t() < min_vals[0])
            min_vals[0] = pt.get_t();
        if (pt.get_t() > max_vals[0])
            max_vals[0] = pt.get_t();
        for (int i = 1; i <= dimension; ++i) {
            if (pt.get_x()(i) < min_vals[i])
                min_vals[i] = pt.get_x()(i);
            if (pt.get_x()(i) > max_vals[i])
                max_vals[i] = pt.get_x()(i);
        }
        if (pt.get_h() < min_vals[dimension1])
            min_vals[dimension1] = pt.get_h();
        if (pt.get_t() > max_vals[dimension1])
            max_vals[dimension1] = pt.get_h();
    }
	SafeLock lock(mutex());
    pts.push_back(pt);
}

void Solution::export_to_scv(const std::string& file_name) const throw (std::exception)
{
	SafeLock lock(mutex());
    static const std::string comma(",");
    std::ofstream of(file_name.c_str());
    
    std::vector<std::string>::const_iterator iv = var_names.begin();
    while (iv != var_names.end()) {
        of << *iv++ << comma;
    }
    of << std::endl;
    of.precision(16);	// fix: all available digits to store
	of.setf(std::ios::scientific | std::ios::showpoint | std::ios::left);
    std::vector<SolutionPoint>::const_iterator ip = pts.begin();
    while (ip != pts.end()) {
        of << ip -> get_t() << comma;
        cvm::rvector::const_iterator ix = ip->get_x().begin();
        while (ix != ip->get_x().end()) {
            of << *ix++ << comma;
        }
        of << ip -> get_h() << std::endl;
        ++ip;
    }
}

void Solution::export_to_matlab(const std::string& file_name, const std::string& var_name) const throw (std::exception)
{
	SafeLock lock(mutex());
    static const std::string space(" ");
    std::ofstream of(file_name.c_str());
    of.precision(16);	// fix: all available digits to store
	of.setf(std::ios::scientific | std::ios::showpoint | std::ios::left);
    
    of << var_name << "=[" << std::endl;
    std::vector<SolutionPoint>::const_iterator ip = pts.begin();
    while (ip != pts.end()) {
        of << ip -> get_t() << space;
        cvm::rvector::const_iterator ix = ip->get_x().begin();
        while (ix != ip->get_x().end()) {
            of << *ix++ << space;
        }
        of << ip -> get_h() << std::endl;
        ++ip;
    }
    of << "];" << std::endl;
    of << "%plot(" << var_name << "(:,1)," << var_name << "(:,2));" << std::endl;
}
