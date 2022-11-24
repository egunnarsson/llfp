#pragma once

#include <algorithm>
#include <limits>
#include <string>
#include <unordered_map>
#include <vector>


namespace llfp
{

template<class T>
T find(const std::unordered_map<std::string, T>& map, const std::string& key, T defaultValue = nullptr)
{
    auto it = map.find(key);
    if (it == map.end())
    {
        return defaultValue;
    }
    return it->second;
}

template<class T, class U>
bool contains(const std::vector<T>& list, const U& value)
{
    return std::any_of(list.begin(), list.end(), [&value](const T& item) { return item == value; });
}

template<class T>
constexpr T npos = T(-1);

template<class T>
bool checkIndex(T i)
{
    return npos<T> != i;
}

template<class T, class P>
typename std::vector<T>::size_type findIndex(const std::vector<T>& list, const P& pred)
{
    auto it = std::find_if(list.begin(), list.end(), pred);
    if (it == list.end()) { return npos<typename std::vector<T>::size_type>; }
    auto distance = std::distance(list.begin(), it);
    return static_cast<std::vector<T>::size_type>(distance);
}

namespace detail
{

template<typename R>
struct enumerator_iter
{
    using range_iterator = decltype(std::begin(std::declval<R&>()));
    using value_pointer  = typename range_iterator::pointer;

    struct result_type
    {
        size_t        index = 0;
        value_pointer value = nullptr;
    };

    enumerator_iter(size_t index, range_iterator it)
        : result_{ index, it.operator->() },
          it_{ it }
    {}
    enumerator_iter(range_iterator it)
        : result_{ std::numeric_limits<size_t>::max(), nullptr },
          it_{ it }
    {}

    const result_type& operator*() const { return result_; }

    enumerator_iter& operator++()
    {
        ++it_;
        ++result_.index;
        result_.value = it_.operator->();
        return *this;
    }

    bool operator==(const enumerator_iter& RHS) const
    {
        return this->it_ == RHS.it_;
    }
    bool operator!=(const enumerator_iter& RHS) const
    {
        return this->it_ != RHS.it_;
    }

private:

    result_type    result_;
    range_iterator it_;
};

template<typename R>
struct enumerator
{
    explicit enumerator(R&& range)
        : range_{ std::forward<R>(range) }
    {}

    enumerator_iter<R> begin() const
    {
        return range_.begin() != range_.end() ? enumerator_iter<R>{ 0, range_.begin() } : enumerator_iter<R>{ range_.end() };
    }

    enumerator_iter<R> end() const
    {
        return enumerator_iter<R>{ range_.end() };
    }

private:

    R range_;
};

} // namespace detail

template<typename R>
detail::enumerator<R> enumerate(R&& TheRange)
{
    return detail::enumerator<R>(std::forward<R>(TheRange));
}

} // namespace llfp
