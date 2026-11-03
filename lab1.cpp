#include <iostream>
#include <complex>
#include <cmath>
#include <stdexcept>
#include <random>
#include <type_traits>
#include <cstddef>

template <typename T>
struct point {
    T x{};
    T y{};

    point() = default;
    point(T x_, T y_) : x(x_), y(y_) {}
};

template <typename T>
struct is_complex : std::false_type {};

template <typename U>
struct is_complex<std::complex<U>> : std::true_type {};

template <typename T>
inline constexpr bool is_complex_v = is_complex<T>::value;

template <typename T>
struct base_type {
    using type = T;
};

template <typename U>
struct base_type<std::complex<U>> {
    using type = U;
};

template <typename T>
using coord_value_t = typename base_type<T>::type;

template <typename T>
coord_value_t<T> coord_abs_diff(const T& a, const T& b) {
    if constexpr (is_complex_v<T>) {
        return std::abs(a - b);
    }
    else {
        if constexpr (std::is_same_v<coord_value_t<T>, int>) {
            return std::abs(a - b);
        }
        else {
            return std::fabs(a - b);
        }
    }
}

template <typename T>
double point_distance(const point<T>& a, const point<T>& b) {
    if constexpr (is_complex_v<T>) {
        using U = typename T::value_type;
        U dx = std::abs(a.x - b.x);
        U dy = std::abs(a.y - b.y);
        return static_cast<double>(std::sqrt(dx * dx + dy * dy));
    }
    else {
        double dx = static_cast<double>(a.x) - static_cast<double>(b.x);
        double dy = static_cast<double>(a.y) - static_cast<double>(b.y);
        return std::sqrt(dx * dx + dy * dy);
    }
}

template <typename T, typename Gen>
T make_coord(Gen& gen, double m1, double m2) {
    if constexpr (std::is_same_v<T, int>) {
        std::uniform_int_distribution<int> dist(
            static_cast<int>(std::ceil(m1)),
            static_cast<int>(std::floor(m2)));
        return dist(gen);
    }
    else if constexpr (std::is_same_v<T, float> ||
        std::is_same_v<T, double>) {
        std::uniform_real_distribution<T> dist(
            static_cast<T>(m1), static_cast<T>(m2));
        return dist(gen);
    }
    else if constexpr (is_complex_v<T>) {
        using U = typename T::value_type;
        std::uniform_real_distribution<U> dist(
            static_cast<U>(m1), static_cast<U>(m2));
        return T(dist(gen), dist(gen));
    }
    else {
        static_assert(sizeof(T) == 0, "make_coord: неподдерживаемый тип");
    }
}

template <typename T>
class polyline {
public:
    using point_type = point<T>;
    using coord_type = coord_value_t<T>;

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
        for (std::size_t i = 0; i < size_; ++i) {
            data_[i].x = make_coord<T>(gen, m1, m2);
            data_[i].y = make_coord<T>(gen, m1, m2);
        }
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
        return coord_abs_diff(a, b) <= eps;
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

    if constexpr (is_complex_v<T>) {
        using U = typename T::value_type;
        U ua = static_cast<U>(a);
        U ub = static_cast<U>(b);
        U uh = static_cast<U>(h);

        point<T> p0 = start;
        point<T> p1(start.x + T(ub, U{ 0 }), start.y);
        point<T> p2(start.x + T(ub, U{ 0 }), start.y + T(uh, U{ 0 }));
        point<T> p3(start.x + T(ua, U{ 0 }), start.y + T(uh, U{ 0 }));

        polyline<T> trap;
        trap += p0;
        trap += p1;
        trap += p2;
        trap += p3;
        trap += p0;
        return trap;
    }
    else {
        T ta = static_cast<T>(a);
        T tb = static_cast<T>(b);
        T th = static_cast<T>(h);

        point<T> p0 = start;
        point<T> p1(start.x + tb, start.y);
        point<T> p2(start.x + tb, start.y + th);
        point<T> p3(start.x + ta, start.y + th);

        polyline<T> trap;
        trap += p0;
        trap += p1;
        trap += p2;
        trap += p3;
        trap += p0;
        return trap;
    }
}

int main() {
    try {
        std::cout << "Задача: прямоугольная трапеция\n";
        point<int> start(0, 0);
        auto trap = make_right_trapezoid(start, 4, 8, 3);
        std::cout << trap << "\n";
        std::cout << "длина = " << trap.length() << "\n\n";

        std::cout << "Случайная ломаная (int)\n";
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

        std::cout << "\nСлучайная ломаная (double)\n";
        polyline<double> d1(4, 0.0, 1.0);
        std::cout << "d1 = " << d1 << "\n";
        std::cout << "d1.length() = " << d1.length() << "\n";

        std::cout << "\nСлучайная ломаная (complex<double>)\n";
        polyline<std::complex<double>> c1(3, -2.0, 2.0);
        std::cout << "c1 = " << c1 << "\n";
        std::cout << "c1.length() = " << c1.length() << "\n";

        std::cout << "\nСравнение complex<double> с учётом мнимой части\n";
        using CD = std::complex<double>;
        polyline<CD> a(point<CD>(CD(1.0, 0.0), CD(2.0, 0.0)));
        polyline<CD> b(point<CD>(CD(1.0, 1e-12), CD(2.0, 0.0)));
        polyline<CD> c(point<CD>(CD(1.0, 1.0), CD(2.0, 0.0)));
        std::cout << "a == b (im отличается на 1e-12) ? " << (a == b) << "\n";
        std::cout << "a == c (im отличается на 1)     ? " << (a == c) << "\n";

        std::cout << "\nИсключения\n";

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