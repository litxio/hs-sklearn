
import json
from sklearn.linear_model import LinearRegression


class Interop:

    def test(self):
        return {'hello': 1}

    def train_lr(self, X, y):
        lr = LinearRegression()
        lr.fit(X, y)
        print(lr.coef_)
