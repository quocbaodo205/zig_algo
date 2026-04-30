#include<bits/stdc++.h>
#define all(x) x.begin(), x.end()
#define ff first
#define ss second
using namespace std;
template <typename T>
using bstring = basic_string<T>;
template <typename T>
using matrix = vector<vector<T>>;
typedef unsigned int uint;
typedef unsigned long long ull;
typedef long long ll;
typedef pair<int,int> pii;
typedef pair<ll,ll> pll;
const ll INFL = (1LL<<62)-1;
const int INF = (1<<30)-1;
const double EPS = 1e-7;
const int MOD = 998244353;
const int RANDOM = chrono::high_resolution_clock::now().time_since_epoch().count();
const int MAXN = 1e6+1;

struct node{
    int p, l, r;
};

int main(){
    
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    
    int n;
    cin >> n;

    vector<int> v(n+1);

    vector<node> tree(n+1);

    tree[0] = {0,0,0};

    for(int i = 1; i <= n; i++)
        cin >> v[i];


    for(int i = 1; i <= n; i++){
        tree[i].p = i-1;
        while(v[tree[i].p] > v[i]){
            tree[i].p = tree[tree[i].p].p;
        }
        tree[i].l = tree[tree[i].p].r;
        tree[tree[i].p].r = i;
        tree[tree[i].l].p = i;
    }

    for(int i = 1; i <= n; i++){
        if(tree[i].p == 0)
            cout << i-1 << ' ';
        else cout << tree[i].p-1 << ' ';
    }
    cout << '\n';

    
    return 0;

}
