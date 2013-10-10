'use strict';

angular.module('coffeebarApp', ['firebase', 'ui.router'])
  .config(function ($stateProvider, $urlRouterProvider) {

    $urlRouterProvider.otherwise('/state1');

    $stateProvider
      .state('/', {
        url: '',
        templateUrl: '/views/main.html',
        controller: 'MainCtrl'
      });
  });
