#include <iostream>
#include <vector>

using namespace std;

const int N = 5 * 100000;

vector <int> a[N];

int main(){

	int x;
	char c;
	int id, auth;
	while(cin >> x){
		cin >> c;
		cin >> id >> c >> auth;
		//cout << id << " " << auth << endl;
		a[id].push_back(auth);
	}


	for (int i = 0 ; i < N ; i ++)
		for (int j = 0 ; j < a[i].size() ; j ++)
			for (int k = j + 1 ; k < a[i].size(); k ++)
				cout << a[i][j] << " , " << a[i][k] << endl;
}