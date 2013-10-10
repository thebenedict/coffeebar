'use strict';
/*global Firebase */

angular.module('coffeebarApp')
  .controller('MainCtrl', function ($scope, angularFire) {
    var ref = new Firebase('https://coffeebar.firebaseio.com');
    $scope.orders = [];
    angularFire(ref, $scope, 'orders');

    $scope.currentOrder = {
      customerName: '',
      items: []
    }

    $scope.submitOrder = function() {
      if (!$scope.currentOrder.items.length || !$scope.currentOrder.customerName) { return; }
      console.log('submitting');
      $scope.orders.push($scope.currentOrder);
      $scope.currentOrder = {
        customerName: '',
        items: []
      }
    };

    $scope.addToOrder = function(item) {
      $scope.currentOrder.items.push(item);
    }

    $scope.deleteOrder = function(toRemove) {
      $scope.orders.splice($scope.orders.indexOf(toRemove), 1);
    };

    $scope.menu = [
      {
        name: 'Small latte',
        price: 700
      },
      {
        name: 'Large latte',
        price: 1000
      },
      {
        name: 'Black Tea',
        price: 300
      },
      {
        name: 'Green Tea',
        price: 300
      },
      {
        name: 'Tea with milk',
        price: 500
      }
    ]

  });
