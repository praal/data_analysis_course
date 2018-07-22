#include <iostream>
#include <vector>
#include <queue>
#include <cstdlib>

#include <iomanip>

using namespace std;

const int N = 200000 * 10 ;

vector <int> v[N];

int n = 0;
int aff[N];

pair< pair<int, int>, pair<int, int> > split(string s){
	int x = 0;
	int affx  = 0;
	int y = 0;
	int affy  = 0;
	int tf = 0;

	for (int i = 0 ; i < s.size() ; i ++){
		if(s[i] == ' ')
			continue;
		else if(s[i] == ',')
			tf ++;
		else if(tf == 0)
			x = x * 10 + (int)(s[i] - '0');
		else if(tf == 1)
			affx = affx * 10 + (int)(s[i] - '0');
		else if(tf == 2)
			y = y * 10 + (int)(s[i] - '0');
		else if(tf == 3)
			affy = affy * 10 + (int)(s[i] - '0');


	}
	return make_pair(make_pair(x,affx), make_pair(y, affy));

}


int main(){
	int x,y;
	char c;
	string s;
	pair < pair<int, int>, pair<int, int> > tmp;

	while(getline(cin , s)){
		
		tmp = split(s);
		x = tmp.first.first;
		y = tmp.second.first;
		//cout << x << " " << y << endl;
		v[x].push_back(y);
		v[y].push_back(x);
		n = max(x,n);
		n = max(y,n);
		aff[x] = tmp.first.second;
		aff[y] = tmp.second.second;
		cout << x 
	}

	int tot = 0;
	int h = 0;
	for (int i = 0; i <= n ; i ++){
		for (int j = 0 ; j < v[i].size() ; j ++){
			tot ++;
			y = v[i][j];
			if(aff[i] == aff[y])
				h ++;
		}

	}

	double ans = 1;
	ans *= h;
	ans /= tot;
	ans *= 100;
	cout << ans << endl;

}
