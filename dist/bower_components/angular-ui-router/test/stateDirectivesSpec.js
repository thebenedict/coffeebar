describe('uiStateRef', function() {

  var el, template, scope, document;

  beforeEach(module('ui.router'));

  beforeEach(module(function($stateProvider) {
    $stateProvider.state('index', {
      url: '/'
    }).state('contacts', {
      url: '/contacts',
      template: '<a ui-sref=".item({ id: 5 })" class="item">Person</a> <ui-view></ui-view>'
    }).state('contacts.item', {
      url: '/:id',
      template: '<a ui-sref=".detail" class="item-detail">Detail</a> | <a ui-sref="^" class="item-parent">Parent</a> | <ui-view></ui-view>'
    }).state('contacts.item.detail', {
      template: '<div class="title">Detail</div> | <a ui-sref="^" class="item-parent2">Item</a>'
    });
  }));

  beforeEach(inject(function($document) {
    document = $document[0];
  }));

  function triggerClick(el, options) {
    options = angular.extend({
      metaKey:  false,
      ctrlKey:  false,
      shiftKey: false,
      altKey:   false,
      button:   0
    }, options || {});

    var e = document.createEvent("MouseEvents");
    e.initMouseEvent(
      "click", // typeArg of type DOMString, Specifies the event type.
      true, // canBubbleArg of type boolean, Specifies whether or not the event can bubble.
      true, // cancelableArg of type boolean, Specifies whether or not the event's default action can be prevented.
      undefined, // viewArg of type views::AbstractView, Specifies the Event's AbstractView.
      0, // detailArg of type long, Specifies the Event's mouse click count.
      0, // screenXArg of type long, Specifies the Event's screen x coordinate
      0, // screenYArg of type long, Specifies the Event's screen y coordinate
      0, // clientXArg of type long, Specifies the Event's client x coordinate
      0, // clientYArg of type long, Specifies the Event's client y coordinate
      options.ctrlKey, // ctrlKeyArg of type boolean, Specifies whether or not control key was depressed during the Event.
      options.altKey, // altKeyArg of type boolean, Specifies whether or not alt key was depressed during the Event.
      options.shiftKey, // shiftKeyArg of type boolean, Specifies whether or not shift key was depressed during the Event.
      options.metaKey, // metaKeyArg of type boolean, Specifies whether or not meta key was depressed during the Event.
      options.button, // buttonArg of type unsigned short, Specifies the Event's mouse button.
      null // relatedTargetArg of type EventTarget
    );
    el[0].dispatchEvent(e);
  }

  describe('links', function() {

    beforeEach(inject(function($rootScope, $compile) {
      el = angular.element('<a ui-sref="contacts.item.detail({ id: contact.id })">Details</a>');
      scope = $rootScope;
      scope.contact = { id: 5 };
      scope.$apply();

      $compile(el)(scope);
      scope.$digest();
    }));

    it('should generate the correct href', function() {
      expect(el.attr('href')).toBe('#/contacts/5');
    });

    it('should update the href when parameters change', function() {
      expect(el.attr('href')).toBe('#/contacts/5');
      scope.contact.id = 6;
      scope.$apply();
      expect(el.attr('href')).toBe('#/contacts/6');
    });

    it('should transition states when left-clicked', inject(function($state, $stateParams, $document, $q) {
      expect($state.$current.name).toEqual('');

      triggerClick(el);
      $q.flush();

      expect($state.current.name).toEqual('contacts.item.detail');
      expect($stateParams).toEqual({ id: "5" });
    }));

    it('should not transition states when ctrl-clicked', inject(function($state, $stateParams, $document, $q) {
      expect($state.$current.name).toEqual('');
      triggerClick(el, { ctrlKey: true });

      $q.flush();
      expect($state.current.name).toEqual('');
      expect($stateParams).toEqual({ id: "5" });
    }));

    it('should not transition states when meta-clicked', inject(function($state, $stateParams, $document, $q) {
      expect($state.$current.name).toEqual('');

      triggerClick(el, { metaKey: true });
      $q.flush();

      expect($state.current.name).toEqual('');
      expect($stateParams).toEqual({ id: "5" });
    }));

    it('should not transition states when shift-clicked', inject(function($state, $stateParams, $document, $q) {
      expect($state.$current.name).toEqual('');

      triggerClick(el, { shiftKey: true });
      $q.flush();

      expect($state.current.name).toEqual('');
      expect($stateParams).toEqual({ id: "5" });
    }));

    it('should not transition states when middle-clicked', inject(function($state, $stateParams, $document, $q) {
      expect($state.$current.name).toEqual('');

      triggerClick(el, { button: 1 });
      $q.flush();

      expect($state.current.name).toEqual('');
      expect($stateParams).toEqual({ id: "5" });
    }));
  });

  describe('forms', function() {
    var el, scope;

    beforeEach(inject(function($rootScope, $compile) {
      el = angular.element('<form ui-sref="contacts.item.detail({ id: contact.id })"></form>');
      scope = $rootScope;
      scope.contact = { id: 5 };
      scope.$apply();

      $compile(el)(scope);
      scope.$digest();
    }));

    it('should generate the correct action', function() {
      expect(el.attr('action')).toBe('#/contacts/5');
    });
  });

  describe('relative transitions', function() {

    beforeEach(inject(function($rootScope, $compile, $state) {
      $state.transitionTo("contacts.item", { id: 5 });
      el = angular.element('<a ui-sref=".detail">Details</a>');
      scope = $rootScope;
      scope.$apply();

      $compile(el)(scope);
      template = $compile(angular.element('<ui-view></ui-view>'))(scope);
      scope.$digest();
    }));

    it('should work', inject(function ($state, $stateParams, $q) {
      triggerClick(el);
      $q.flush();

      expect($state.$current.name).toBe("contacts.item.detail");
      expect($state.params).toEqual({ id: '5' });
    }));

    it('should resolve states from parent uiView', inject(function ($state, $stateParams, $q) {
      $state.transitionTo('contacts');
      $q.flush();

      var parentToChild = angular.element(template[0].querySelector('a.item'));
      triggerClick(parentToChild);
      $q.flush();

      var childToGrandchild = angular.element(template[0].querySelector('a.item-detail'));
      var childToParent = angular.element(template[0].querySelector('a.item-parent'));

      triggerClick(childToGrandchild);
      $q.flush();

      var grandchildToParent = angular.element(template[0].querySelector('a.item-parent2'));
      expect($state.$current.name).toBe("contacts.item.detail")

      triggerClick(grandchildToParent);
      $q.flush();
      expect($state.$current.name).toBe("contacts.item");

      $state.transitionTo("contacts.item.detail", { id: 3 });
      triggerClick(childToParent);
      $q.flush();
      expect($state.$current.name).toBe("contacts");
    }));
  });
});
