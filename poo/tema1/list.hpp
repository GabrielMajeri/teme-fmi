#ifndef LIST_HPP
#define LIST_HPP

#include <memory>
#include <utility>

template <typename T>
class SortedList;

/// Nod din lista
template <typename T>
class Node {
    std::unique_ptr<Node> next = nullptr;
    int key;
    T value;

    friend class SortedList<T>;
public:
    Node(int key, const T& value)
        : key(key), value(value) {
    }

    Node(const Node& other)
        : key(other.key), value(other.value) {
    }

    Node& operator=(const Node& other) = delete;

    Node* nextNode() const {
        return next.get();
    }

    /// Adauga un nod dupa acesta
    void linkNode(std::unique_ptr<Node> newNext) {
        newNext->next = std::move(next);
        next = std::move(newNext);
    }

    int getKey() const {
        return key;
    }

    T& getValue() {
        return value;
    }

    const T& getValue() const {
        return value;
    }
};

/// Lista simplu inlantuita care isi pastreaza elementele ordonate dupa o cheie
template <typename T>
class SortedList {
    std::unique_ptr<Node<T>> first = nullptr;

public:
    SortedList() = default;

    SortedList(const SortedList& other) {
        *this = other;
    }

    ~SortedList() {
        clear();
    }

    SortedList& operator=(const SortedList& rhs) {
        if (rhs.first == nullptr) {
            return *this;
        }

        first = std::make_unique<Node<T>>(*rhs.first);

        for (Node<T>* outptr = first.get(), * inptr = rhs.first->nextNode(); inptr != nullptr; inptr = inptr->nextNode()) {
            outptr->next = std::make_unique<Node<T>>(*inptr);
            outptr = outptr->nextNode();
        }

        return *this;
    }

    SortedList& operator=(SortedList&& rhs) {
        first = std::exchange(rhs.first, nullptr);
        return *this;
    }

    Node<T>* begin() {
        return first.get();
    }

    const Node<T>* cbegin() const {
        return first.get();
    }

    bool empty() const {
        return first == nullptr;
    }

    void clear() {
        first = nullptr;
    }

    /// Sterge un nod cu o anumita cheie
    void erase(int key) {
        if (first == nullptr) {
            return;
        }

        if (first->key == key) {
            first = nullptr;
            return;
        }

        for (Node<T>* ptr = first.get(); ptr->next != nullptr; ptr = ptr->nextNode()) {
            if (ptr->next->key == key) {
                ptr->next = std::move(ptr->next->next);
                break;
            }
        }
    }

    /// Adauga un nod cu o anumita cheie (sau suprascrie daca un nod are deja aceasta cheie)
    T& set(int key, const T& value) {
        std::unique_ptr<Node<T>> node = std::make_unique<Node<T>>(key, value);
        T& val = node->value;

        if (first == nullptr) {
            first = std::move(node);
        } else {
            Node<T>* ptr = first.get();

            // Daca trebuie inserat inaintea primului
            if (key < ptr->key) {
                node->next = std::move(first);
                first = std::move(node);
                return first->value;
            } else {
                while (ptr->nextNode() != nullptr && key > ptr->next->key) {
                    ptr = ptr->nextNode();
                }

                if (ptr->next == nullptr) {
                    ptr->next = std::move(node);
                } else {
                    if (key == ptr->next->key) {
                        ptr->next->value = value;
                        return ptr->next->value;
                    } else {
                        node->next = std::move(ptr->next);
                        ptr->next = std::move(node);
                    }
                }
            }
        }

        return val;
    }

    T* get(int key) {
        for (Node<T>* ptr = first.get(); ptr != nullptr; ptr = ptr->nextNode()) {
            if (ptr->key == key) {
                return &ptr->getValue();
            }
        }

        return nullptr;
    }

    const T* get(int key) const {
        for (const Node<T>* ptr = first.get(); ptr != nullptr; ptr = ptr->nextNode()) {
            if (ptr->key == key) {
                return &ptr->getValue();
            }
        }

        return nullptr;
    }
};

#endif
