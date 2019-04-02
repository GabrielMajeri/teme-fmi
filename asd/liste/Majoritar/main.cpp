#include <iostream>
#include <fstream>
#include <unordered_map>

using namespace std;

using HashMap = unordered_map<int, int>;
using Iterator = HashMap::iterator;

int main() {
    HashMap frecv;

    ifstream in("date.in");
    int n;
    in >> n;

    for (int i = 0; i < n; ++i) {
        int x;
        in >> x;

        ++frecv[x];
    }

    int max_it = 0;
    int max_fr = 0;

    for (Iterator it = frecv.begin(); it != frecv.end(); ++it) {
        int elem = it->first;
        int fr = it->second;
        if (fr > max_fr) {
            max_fr = fr;
            max_it = elem;
        }
    }

    if (max_fr > n / 2) {
        cout << "Element majoritar: " << max_it << " apare de " << max_fr << " ori.\n";
    } else {
        cout << "Nu exista element majoritar.\n";
    }
}
