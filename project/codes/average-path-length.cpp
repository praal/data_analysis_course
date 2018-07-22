#include <iostream>
#include <vector>
#include <queue>
#include <cstdlib>
#include<set>

using namespace std;

const int N = 200000 * 10 ;

vector <int> v[N];
int a[N];
bool mark[N];

int n = 0;
long long num  =0;
long long tot = 0;

void bfs(int k){

	if(k % 1000 == 0)
		cout << k << endl;
	queue <pair <int,int> > q;


	q.push(make_pair(k,0));
	for (int i = 0 ; i <= n ; i ++)
		mark[i] = false;

	mark[k] = true;

	while(!q.empty()){
		num ++;
		pair<int,int> p = q.front();
		q.pop();
		int x = p.first;
		for (int i = 0 ; i < v[x].size(); i ++){
			if(!mark[v[x][i]]){
				mark[v[x][i]] = true;
				a[v[x][i]] = p.second + 1;
				q.push(make_pair(v[x][i], p.second + 1));
			}
		}

	}
	num --;
}

pair<int, int> split(string s){
	int x = 0;
	int y  = 0;
	bool tf = true;

	for (int i = 0 ; i < s.size() ; i ++){
		if(s[i] == ' ')
			continue;
		else if(s[i] == ',')
			tf = false;
		else if(tf)
			x = x * 10 + (int)(s[i] - '0');
		else
			y = y * 10 + (int)(s[i] - '0');

	}
	return make_pair(x,y);

}
void init(){
	for (int i = 0 ; i <= n ; i++)
		a[i] = 0;

}
void calc(){
	for (int i = 0 ;i <= n ; i ++){
		tot += a[i];
	}
}
int main(){
	int x,y;
	char c;
	string s;
	pair<int, int> tmp;

	while(getline(cin , s)){
		
		tmp = split(s);
		x = tmp.first;
		y = tmp.second;
		//cout << x << " " << y << endl;
		v[x].push_back(y);
		v[y].push_back(x);
		n = max(x,n);
		n = max(y,n);
	}


	for (int i = 0 ;i <= n ; i ++){
		if (i % 1000 == 0)
			cout << i << " " << n << endl;
		init();
		bfs(i);	
		calc();
	}


	double ans = 0;



	num /= 2;
	ans = tot;
	ans /= num;

	cout << "number of paths: " << num << " sum of paths: " << tot << " average: " <<  ans << endl;


}
