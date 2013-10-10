'use strict';
/*global Firebase */

angular.module('coffeebarApp')
  .controller('MainCtrl', function ($scope, angularFire) {
    var ref = new Firebase('https://coffeebar.firebaseio.com');
    $scope.orders = [];
    angularFire(ref, $scope, 'orders');

    $scope.submitOrder = function(e) {
      if (e.keyCode !== 13) { return; }
      $scope.orders.push({name: $scope.name, order: $scope.order});
      $scope.order = '';
    };

    $scope.deleteOrder = function(toRemove) {
      $scope.orders.splice(toRemove, 1);
    };

    $scope.awesomeThings = ['foo', 'bar', 'baz'];

  });
