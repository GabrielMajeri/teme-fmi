#ifndef LIST_HPP
#define LIST_HPP

template <typename T>
class SortedList;

/// Nod din lista, retine o cheie si o valoare
template <typename T>
class Node {
    Node* next;

    int key;
    T value;

    friend class SortedList<T>;

public:
    Node(int key, const T& value)
        : next(nullptr), key(key), value(value) {
    }

    Node(const Node& other)
        : next(nullptr), key(other.key), value(other.value) {
    }


    Node& operator=(const Node& other) {
        key = other.key;
        value = other.value;
        next = nullptr;
        return *this;
    }


    bool operator==(const Node& rhs) const {
        return key == rhs.key && value == rhs.value;
    }

    bool operator!=(const Node& rhs) const {
        return !(*this == rhs);
    }


    Node* nextNode() const {
        return next;
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
    Node<T>* first;

public:
    SortedList()
        : first(nullptr) {
    }

    SortedList(const SortedList& other)
        : first(nullptr) {
        *this = other;
    }

    ~SortedList() {
        clear();
    }

    /// Copiaza valorile nodurilor din alta lista
    SortedList& operator=(const SortedList& rhs) {
        if (this == &rhs) {
            return *this;
        }

        clear();

        if (rhs.first == nullptr) {
            return *this;
        }

        first = new Node<T>(*rhs.first);

        Node<T>* outPtr = first;
        const Node<T>* inPtr = rhs.first->next;

        while (inPtr) {
            outPtr->next = new Node<T>(*inPtr);
            inPtr = inPtr->next;
            outPtr = outPtr->next;
        }

        return *this;
    }

    /// Verifica ca doua liste sunt egale
    bool operator==(const SortedList& rhs) const {
        const Node<T>* pa = cbegin();
        const Node<T>* pb = rhs.cbegin();

        while (pa && pb) {
            if (pa->getKey() != pb->getKey() ||
                pa->getValue() != pb->getValue()) {
                return false;
            }

            pa = pa->nextNode();
            pb = pb->nextNode();
        }

        // Daca o lista are elemente in plus, atunci ele difera
        if (pa || pb) {
            return false;
        }

        return true;
    }

    bool operator!=(const SortedList& rhs) const {
        return !(*this == rhs);
    }


    /// Sterge toate nodurile din lista
    void clear() {
        Node<T>* ptr = first;
        while (ptr != nullptr) {
            Node<T>* next = ptr->next;
            delete ptr;
            ptr = next;
        }
        first = nullptr;
    }

    Node<T>* begin() {
        return first;
    }

    const Node<T>* cbegin() const {
        return first;
    }

    /// Verifica daca lista este goala
    bool empty() const {
        return first == nullptr;
    }

    /// Sterge un nod cu o anumita cheie, daca exista
    void erase(int key) {
        if (first == nullptr) {
            return;
        }

        if (first->key == key) {
            delete first;
            first = nullptr;
            return;
        }

        for (Node<T>* ptr = first; ptr->next != nullptr; ptr = ptr->next) {
            if (ptr->next->key == key) {
                Node<T>* newNext = ptr->next->next;
                delete ptr->next;
                ptr->next = newNext;
                break;
            }
        }
    }

    /// Adauga un nod cu o anumita cheie (sau suprascrie daca un nod are deja aceasta cheie)
    void set(int key, const T& value) {
        if (first == nullptr) {
            first = new Node<T>(key, value);
            return;
        }

        // Daca trebuie inserat inaintea primului
        if (key < first->key) {
            Node<T>* newFirst = new Node<T>(key, value);
            newFirst->next = first;
            first = newFirst;
            return;
        }

        // Daca trebuie suprascris primul
        if (key == first->key) {
            first->value = value;
            return;
        }

        Node<T>* ptr = first;

        while (ptr->next != nullptr && key > ptr->next->key) {
            ptr = ptr->next;
        }

        // Nodul trebuie inserat la final
        if (ptr->next == nullptr) {
            ptr->next = new Node<T>(key, value);
            return;
        }

        // Daca trebuie suprascris nodul
        if (ptr->next->key == key) {
            ptr->next->value = value;
            return;
        }

        Node<T>* newNode = new Node<T>(key, value);
        newNode->next = ptr->next;
        ptr->next = newNode;
    }

    T* get(int key) {
        for (Node<T>* ptr = first; ptr != nullptr; ptr = ptr->next) {
            if (ptr->key == key) {
                return &ptr->getValue();
            }
        }

        return nullptr;
    }

    const T* get(int key) const {
        for (const Node<T>* ptr = first; ptr != nullptr; ptr = ptr->next) {
            if (ptr->key == key) {
                return &ptr->getValue();
            }
        }

        return nullptr;
    }

    friend std::ostream& operator<<(std::ostream& os, const SortedList& l) {
        const Node<T>* ptr = l.first;
        while (ptr) {
            os << "(" << ptr->getKey() << ", " << ptr->getValue() << ") ";
            ptr = ptr->nextNode();
        }
        return os;
    }
};

#endif
