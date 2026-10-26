#include <iostream>
#include <complex>
#include <cmath>
#include <stdexcept>
#include <random>
#include <type_traits>
#include <initializer_list>
#include <cstddef>

template <typename T>
struct point {
    T x{};
    T y{};

    point() = default;
    point(T x_, T y_) : x(x_), y(y_) {}
};

template <typename T>
double point_distance(const point<T>& a, const point<T>& b) {
    double dx = static_cast<double>(a.x) - static_cast<double>(b.x);
    double dy = static_cast<double>(a.y) - static_cast<double>(b.y);
    return std::sqrt(dx * dx + dy * dy);
}

template <typename U>
double point_distance(const point<std::complex<U>>& a,
    const point<std::complex<U>>& b) {
    return std::sqrt(std::norm(a.x - b.x) + std::norm(a.y - b.y));
}

template <typename T>
T make_coord(double v) {
    if constexpr (std::is_same_v<T, int>) {
        return static_cast<T>(std::llround(v));
    }
    else if constexpr (std::is_same_v<T, float> ||
        std::is_same_v<T, double>) {
        return static_cast<T>(v);
    }
    else {
        using U = typename T::value_type;
        return T(static_cast<U>(v), U{ 0 });
    }
}

template <typename T>
double to_double(const T& v) {
    if constexpr (std::is_same_v<T, std::complex<float>> ||
        std::is_same_v<T, std::complex<double>>) {
        return static_cast<double>(v.real());
    }
    else {
        return static_cast<double>(v);
    }
}

template <typename T>
class polyline {
public:
    using point_type = point<T>;

    static constexpr double eps = 1e-9;

private:
    point_type* data_;
    std::size_t size_;

    void allocate(std::size_t n) {
        data_ = n ? new point_type[n] : nullptr;
        size_ = n;
    }

    void copy_from(const polyline& other) {
        allocate(other.size_);
        for (std::size_t i = 0; i < size_; ++i) data_[i] = other.data_[i];
    }

    void release() noexcept {
        delete[] data_;
        data_ = nullptr;
        size_ = 0;
    }

public:
    polyline() : data_(nullptr), size_(0) {}

    explicit polyline(const point_type& p) : data_(nullptr), size_(0) {
        allocate(1);
        data_[0] = p;
    }

    explicit polyline(std::size_t n) : data_(nullptr), size_(0) {
        allocate(n);
    }

    polyline(std::size_t n, double m1, double m2) : data_(nullptr), size_(0) {
        if (m1 > m2) throw std::invalid_argument("ломаная: нижняя граница больше верхней");
        allocate(n);
        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_real_distribution<double> dist(m1, m2);
        for (std::size_t i = 0; i < size_; ++i) {
            data_[i].x = make_coord<T>(dist(gen));
            data_[i].y = make_coord<T>(dist(gen));
        }
    }

    polyline(std::initializer_list<point_type> init) : data_(nullptr), size_(0) {
        allocate(init.size());
        std::size_t i = 0;
        for (const auto& p : init) data_[i++] = p;
    }

    polyline(const polyline& other) : data_(nullptr), size_(0) {
        copy_from(other);
    }

    polyline& operator=(const polyline& other) {
        if (this != &other) {
            release();
            copy_from(other);
        }
        return *this;
    }

    ~polyline() { release(); }

    point_type& operator[](std::size_t i) {
        if (i >= size_) throw std::out_of_range("ломаная: индекс за пределами диапазона");
        return data_[i];
    }
    const point_type& operator[](std::size_t i) const {
        if (i >= size_) throw std::out_of_range("ломаная: индекс за пределами диапазона");
        return data_[i];
    }

    std::size_t size() const noexcept { return size_; }
    bool empty() const noexcept { return size_ == 0; }

    polyline operator+(const polyline& other) const {
        polyline result;
        result.allocate(size_ + other.size_);
        for (std::size_t i = 0; i < size_; ++i) result.data_[i] = data_[i];
        for (std::size_t i = 0; i < other.size_; ++i)
            result.data_[size_ + i] = other.data_[i];
        return result;
    }

    polyline operator+(const point_type& p) const {
        polyline result;
        result.allocate(size_ + 1);
        for (std::size_t i = 0; i < size_; ++i) result.data_[i] = data_[i];
        result.data_[size_] = p;
        return result;
    }

    friend polyline operator+(const point_type& p, const polyline& pl) {
        polyline result;
        result.allocate(pl.size_ + 1);
        result.data_[0] = p;
        for (std::size_t i = 0; i < pl.size_; ++i)
            result.data_[i + 1] = pl.data_[i];
        return result;
    }

    polyline& operator+=(const polyline& other) { return *this = *this + other; }
    polyline& operator+=(const point_type& p) { return *this = *this + p; }

    double length() const {
        double total = 0.0;
        for (std::size_t i = 1; i < size_; ++i)
            total += point_distance(data_[i - 1], data_[i]);
        return total;
    }

    static bool coord_eq(const T& a, const T& b) {
        return std::fabs(to_double(a) - to_double(b)) <= eps;
    }

    bool operator==(const polyline& other) const {
        if (size_ != other.size_) return false;
        for (std::size_t i = 0; i < size_; ++i) {
            if (!coord_eq(data_[i].x, other.data_[i].x)) return false;
            if (!coord_eq(data_[i].y, other.data_[i].y)) return false;
        }
        return true;
    }

    bool operator!=(const polyline& other) const { return !(*this == other); }

    friend std::ostream& operator<<(std::ostream& os, const polyline& pl) {
        os << "ломаная[" << pl.size_ << "]: ";
        for (std::size_t i = 0; i < pl.size_; ++i) {
            os << "(" << pl.data_[i].x << ", " << pl.data_[i].y << ")";
            if (i + 1 < pl.size_) os << " -> ";
        }
        return os;
    }
};

template <typename T>
polyline<T> make_right_trapezoid(const point<T>& start,
    double a, double b, double h)
{
    if (a < 0 || b < 0 || h < 0)
        throw std::invalid_argument("прямоугольная трапеция: отрицательный параметр");

    double sx = to_double(start.x);
    double sy = to_double(start.y);

    polyline<T> trap;
    trap += point<T>(make_coord<T>(sx), make_coord<T>(sy));
    trap += point<T>(make_coord<T>(sx + b), make_coord<T>(sy));
    trap += point<T>(make_coord<T>(sx + b), make_coord<T>(sy + h));
    trap += point<T>(make_coord<T>(sx + a), make_coord<T>(sy + h));
    trap += point<T>(make_coord<T>(sx), make_coord<T>(sy));
    return trap;
}

int main() {
    try {
        std::cout << "Прямоугольная трапеция \n";
        point<int> start(0, 0);
        auto trap = make_right_trapezoid(start, 4, 8, 3);
        std::cout << trap << "\n";
        std::cout << "длина = " << trap.length() << "\n";

        std::cout << "Демонстрация на случайном объекте (int) \n";
        polyline<int> r1(5, -10.0, 10.0);
        std::cout << "r1 = " << r1 << "\n";
        std::cout << "r1.length() = " << r1.length() << "\n";

        polyline<int> r2(3, 0.0, 5.0);
        std::cout << "r2 = " << r2 << "\n";

        std::cout << "r1 + r2 = " << (r1 + r2) << "\n";
        point<int> extra(100, 100);
        std::cout << "r1 + точка = " << (r1 + extra) << "\n";
        std::cout << "точка + r2 = " << (extra + r2) << "\n";

        polyline<int> r3 = r1;
        std::cout << "r1 == r3 ? " << (r1 == r3) << "\n";
        std::cout << "r1 != r2 ? " << (r1 != r2) << "\n";

        r3 += extra;
        std::cout << "r3 (после +=) = " << r3 << "\n";

        std::cout << "\n Демонстрация на случайном объекте (double) \n";
        polyline<double> d1(4, 0.0, 1.0);
        std::cout << "d1 = " << d1 << "\n";
        std::cout << "d1.length() = " << d1.length() << "\n";

        std::cout << "\n Демонстрация на случайном объекте (complex<double>) \n";
        polyline<std::complex<double>> c1(3, -2.0, 2.0);
        std::cout << "c1.length() = " << c1.length() << "\n";

        std::cout << "\n Исключения \n";

        try {
            auto bad = r1[100];
            (void)bad;
        }
        catch (const std::out_of_range& e) {
            std::cout << "out_of_range: " << e.what() << "\n";
        }

        try {
            polyline<int> bad(std::size_t{ 3 }, 1.0, -1.0);
            (void)bad;
        }
        catch (const std::invalid_argument& e) {
            std::cout << "invalid_argument: " << e.what() << "\n";
        }

        try {
            make_right_trapezoid(start, -1.0, 2.0, 3.0);
        }
        catch (const std::invalid_argument& e) {
            std::cout << "invalid_argument: " << e.what() << "\n";
        }
    }
    catch (const std::exception& e) {
        std::cerr << "Непредвиденная ошибка: " << e.what() << "\n";
        return 1;
    }
    return 0;
}
