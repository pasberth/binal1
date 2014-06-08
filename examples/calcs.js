(function () {
    'use strict';
    var Binal;
    Binal = require('binal-runtime');
    var S;
    S = function (x) {
        return function (y) {
            return function (z) {
                if (!(arguments.length === 1))
                    z = new Binal.Tuple(Array.prototype.slice.call(arguments, 0));
                var _tmp0, _tmp1;
                if (z instanceof Binal.Tuple)
                    _tmp0 = x.apply(this, z.xs);
                else
                    _tmp0 = x(z);
                if (z instanceof Binal.Tuple)
                    _tmp1 = y.apply(this, z.xs);
                else
                    _tmp1 = y(z);
                if (_tmp1 instanceof Binal.Tuple)
                    return _tmp0.apply(this, _tmp1.xs);
                else
                    return _tmp0(_tmp1);
            };
        };
    };
    var K;
    K = function (x) {
        if (!(arguments.length === 1))
            x = new Binal.Tuple(Array.prototype.slice.call(arguments, 0));
        return function (y) {
            if (!(arguments.length === 1))
                y = new Binal.Tuple(Array.prototype.slice.call(arguments, 0));
            return x;
        };
    };
    var I;
    I = S(K)(K);
    var B;
    B = S(K(S))(K);
    var C;
    C = S(S(K(S(K(S))(K)))(S))(K(K));
    var W;
    W = S(S)(S(K));
    var Y;
    return Y = function (f) {
        return function (x) {
            if (!(arguments.length === 1))
                x = new Binal.Tuple(Array.prototype.slice.call(arguments, 0));
            var _tmp0;
            if (x instanceof Binal.Tuple)
                _tmp0 = x.apply(this, x.xs);
            else
                _tmp0 = x(x);
            if (_tmp0 instanceof Binal.Tuple)
                return f.apply(this, _tmp0.xs);
            else
                return f(_tmp0);
        }(function (x) {
            if (!(arguments.length === 1))
                x = new Binal.Tuple(Array.prototype.slice.call(arguments, 0));
            var _tmp0;
            if (x instanceof Binal.Tuple)
                _tmp0 = x.apply(this, x.xs);
            else
                _tmp0 = x(x);
            if (_tmp0 instanceof Binal.Tuple)
                return f.apply(this, _tmp0.xs);
            else
                return f(_tmp0);
        });
    };
}());
