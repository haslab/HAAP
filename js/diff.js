var app = angular.module('diffDemo', ['ngMaterial', 'diff-match-patch']);
app.config(function($mdThemingProvider) {
  $mdThemingProvider.theme('default')
    .primaryPalette('indigo')
    .accentPalette('blue');
});

var src = document.getElementById("sourceTxt");
var tgt = document.getElementById("targetTxt");

app.controller('diffCtrl', ['$scope', function($scope) {
    $scope.left = src.innerHTML;

    $scope.right = tgt.innerHTML;

  $scope.source = false;
  $scope.target = false;
  $scope.diff = true;

  $scope.options = {
    editCost: 4,
    interLineDiff: true,
    ignoreTrailingNewLines: true,
    attrs: {
      insert: {
        'data-attr': 'insert',
        'class': 'insertion'
      },
      delete: {
        'data-attr': 'delete'
      },
      equal: {
        'data-attr': 'equal'
      }
    }
  };
  
  $scope.setClickedSource = function(){  
      
      var txt = document.getElementById("sourceTxt");
      if (!$scope.source) {
          txt.style.display = "block";
      }
      else {
          txt.style.display = "none";
      }
  }
  
  $scope.setClickedTarget = function(){  
      
      var txt = document.getElementById("targetTxt");
      if (!$scope.target) {
          txt.style.display = "block";
      }
      else {
          txt.style.display = "none";
      }
  }
  
  $scope.setClickedDiff = function(){  
      
      var txt = document.getElementById("diffTxt");
      if (!$scope.diff) {
          txt.style.display = "block";
      }
      else {
          txt.style.display = "none";
      }
  }
  
  
  
}]);

