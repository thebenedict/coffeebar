!function(a,b,c){"use strict";function d(a,b){return y(new(y(function(){},{prototype:a})),b)}function e(a){return x(arguments,function(b){b!==a&&x(b,function(b,c){a.hasOwnProperty(c)||(a[c]=b)})}),a}function f(a,b){var c=[];for(var d in a.path)if(""!==a.path[d]){if(!b.path[d])break;c.push(a.path[d])}return c}function g(a,b,c,d){var e,g=f(c,d),h={},i=[];for(var j in g)if(g[j].params&&g[j].params.length){e=g[j].params;for(var k in e)i.indexOf(e[k])>=0||(i.push(e[k]),h[e[k]]=a[e[k]])}return y({},h,b)}function h(a,b){var d=1,f=2,g={},h=[],i=g,j=y(a.when(g),{$$promises:g,$$values:g});this.study=function(g){function k(a,c){if(o[c]!==f){if(n.push(c),o[c]===d)throw n.splice(0,n.indexOf(c)),new Error("Cyclic dependency: "+n.join(" -> "));if(o[c]=d,u(a))m.push(c,[function(){return b.get(c)}],h);else{var e=b.annotate(a);x(e,function(a){a!==c&&g.hasOwnProperty(a)&&k(g[a],a)}),m.push(c,a,e)}n.pop(),o[c]=f}}function l(a){return v(a)&&a.then&&a.$$promises}if(!v(g))throw new Error("'invocables' must be an object");var m=[],n=[],o={};return x(g,k),g=n=o=null,function(d,f,g){function h(){--t||(u||e(r,f.$$values),p.$$values=r,p.$$promises=!0,o.resolve(r))}function k(a){p.$$failure=a,o.reject(a)}function n(c,e,f){function i(a){l.reject(a),k(a)}function j(){if(!s(p.$$failure))try{l.resolve(b.invoke(e,g,r)),l.promise.then(function(a){r[c]=a,h()},i)}catch(a){i(a)}}var l=a.defer(),m=0;f.forEach(function(a){q.hasOwnProperty(a)&&!d.hasOwnProperty(a)&&(m++,q[a].then(function(b){r[a]=b,--m||j()},i))}),m||j(),q[c]=l.promise}if(l(d)&&g===c&&(g=f,f=d,d=null),d){if(!v(d))throw new Error("'locals' must be an object")}else d=i;if(f){if(!l(f))throw new Error("'parent' must be a promise returned by $resolve.resolve()")}else f=j;var o=a.defer(),p=o.promise,q=p.$$promises={},r=y({},d),t=1+m.length/3,u=!1;if(s(f.$$failure))return k(f.$$failure),p;f.$$values?(u=e(r,f.$$values),h()):(y(q,f.$$promises),f.then(h,k));for(var w=0,x=m.length;x>w;w+=3)d.hasOwnProperty(m[w])?h():n(m[w],m[w+1],m[w+2]);return p}},this.resolve=function(a,b,c,d){return this.study(a)(b,c,d)}}function i(a,b,c){this.fromConfig=function(a,b,c){return s(a.template)?this.fromString(a.template,b):s(a.templateUrl)?this.fromUrl(a.templateUrl,b):s(a.templateProvider)?this.fromProvider(a.templateProvider,b,c):null},this.fromString=function(a,b){return t(a)?a(b):a},this.fromUrl=function(c,d){return t(c)&&(c=c(d)),null==c?null:a.get(c,{cache:b}).then(function(a){return a.data})},this.fromProvider=function(a,b,d){return c.invoke(a,null,d||{params:b})}}function j(a){function b(b){if(!/^\w+(-+\w+)*$/.test(b))throw new Error("Invalid parameter name '"+b+"' in pattern '"+a+"'");if(f[b])throw new Error("Duplicate parameter name '"+b+"' in pattern '"+a+"'");f[b]=!0,j.push(b)}function c(a){return a.replace(/[\\\[\]\^$*+?.()|{}]/g,"\\$&")}var d,e=/([:*])(\w+)|\{(\w+)(?:\:((?:[^{}\\]+|\\.|\{(?:[^{}\\]+|\\.)*\})+))?\}/g,f={},g="^",h=0,i=this.segments=[],j=this.params=[];this.source=a;for(var k,l,m;(d=e.exec(a))&&(k=d[2]||d[3],l=d[4]||("*"==d[1]?".*":"[^/]*"),m=a.substring(h,d.index),!(m.indexOf("?")>=0));)g+=c(m)+"("+l+")",b(k),i.push(m),h=e.lastIndex;m=a.substring(h);var n=m.indexOf("?");if(n>=0){var o=this.sourceSearch=m.substring(n);m=m.substring(0,n),this.sourcePath=a.substring(0,h+n),x(o.substring(1).split(/[&?]/),b)}else this.sourcePath=a,this.sourceSearch="";g+=c(m)+"$",i.push(m),this.regexp=new RegExp(g),this.prefix=i[0]}function k(){this.compile=function(a){return new j(a)},this.isMatcher=function(a){return v(a)&&t(a.exec)&&t(a.format)&&t(a.concat)},this.$get=function(){return this}}function l(a){function b(a){var b=/^\^((?:\\[^a-zA-Z0-9]|[^\\\[\]\^$*+?.()|{}]+)*)/.exec(a.source);return null!=b?b[1].replace(/\\(.)/g,"$1"):""}function c(a,b){return a.replace(/\$(\$|\d{1,2})/,function(a,c){return b["$"===c?0:Number(c)]})}function d(a,b,c){if(!c)return!1;var d=a.invoke(b,b,{$match:c});return s(d)?d:!0}var e=[],f=null;this.rule=function(a){if(!t(a))throw new Error("'rule' must be a function");return e.push(a),this},this.otherwise=function(a){if(u(a)){var b=a;a=function(){return b}}else if(!t(a))throw new Error("'rule' must be a function");return f=a,this},this.when=function(e,f){var g,h=u(f);if(u(e)&&(e=a.compile(e)),!h&&!t(f)&&!w(f))throw new Error("invalid 'handler' in when()");var i={matcher:function(b,c){return h&&(g=a.compile(c),c=["$match",function(a){return g.format(a)}]),y(function(a,e){return d(a,c,b.exec(e.path(),e.search()))},{prefix:u(b.prefix)?b.prefix:""})},regex:function(a,e){if(a.global||a.sticky)throw new Error("when() RegExp must not be global or sticky");return h&&(g=e,e=["$match",function(a){return c(g,a)}]),y(function(b,c){return d(b,e,a.exec(c.path()))},{prefix:b(a)})}},j={matcher:a.isMatcher(e),regex:e instanceof RegExp};for(var k in j)if(j[k])return this.rule(i[k](e,f));throw new Error("invalid 'what' in when()")},this.$get=["$location","$rootScope","$injector",function(a,b,c){function d(){function b(b){var d=b(c,a);return d?(u(d)&&a.replace().url(d),!0):!1}var d,g=e.length;for(d=0;g>d;d++)if(b(e[d]))return;f&&b(f)}return b.$on("$locationChangeSuccess",d),{}}]}function m(a,e,f){function h(a,b){var d=u(a),e=d?a:a.name,f=0===e.indexOf(".")||0===e.indexOf("^");if(f){if(!b)throw new Error("No reference point given for path '"+e+"'");for(var g=e.split("."),h=0,i=g.length,j=b;i>h;h++)if(""!==g[h]||0!==h){if("^"!==g[h])break;if(!j.parent)throw new Error("Path '"+e+"' not valid for state '"+b.name+"'");j=j.parent}else j=b;g=g.slice(h).join("."),e=j.name+(j.name&&g?".":"")+g}var k=q[e];return!k||!d&&(d||k!==a&&k.self!==a)?c:k}function i(b){b=d(b,{self:b,resolve:b.resolve||{},toString:function(){return this.name}});var c=b.name;if(!u(c)||c.indexOf("@")>=0)throw new Error("State must have a valid name");if(q[c])throw new Error("State '"+c+"'' is already defined");for(var e in r)b[e]=r[e](b);return q[c]=b,!b["abstract"]&&b.url&&a.when(b.url,["$match","$stateParams",function(a,c){p.$current.navigable==b&&m(a,c)||p.transitionTo(b,a,!1)}]),b}function j(a,b){return v(a)?b=a:b.name=a,i(b),this}function k(a,b,e,i,j,k,q){function r(a,c,d,f,g){var h=d?c:n(a.params,c),i={$stateParams:h};g.resolve=j.resolve(a.resolve,i,g.resolve,a);var k=[g.resolve.then(function(a){g.globals=a})];return f&&k.push(f),x(a.views,function(b,c){var d=b.resolve&&b.resolve!==a.resolve?b.resolve:{};d.$template=[function(){return e.load(c,{view:b,locals:i,params:h,notify:!1})||""}],k.push(j.resolve(d,i,g.resolve,a).then(function(d){d.$$controller=b.controller,d.$$state=a,g[c]=d}))}),b.all(k).then(function(){return g})}var t=b.reject(new Error("transition superseded")),u=b.reject(new Error("transition prevented"));return o.locals={resolve:null,globals:{$stateParams:{}}},p={params:{},current:o.self,$current:o,transition:null},p.go=function(a,b,c){return this.transitionTo(a,b,y({inherit:!0,relative:p.$current},c))},p.transitionTo=function(c,e,f){s(f)||(f=f===!0||f===!1?{location:f}:{}),e=e||{},f=y({location:!0,inherit:!1,relative:null},f);var j=h(c,f.relative);if(!s(j))throw new Error("No such state "+j);if(j["abstract"])throw new Error("Cannot transition to abstract state '"+c+"'");f.inherit&&(e=g(k,e||{},p.$current,j)),c=j;var n,v,w=c.path,x=p.$current,A=p.params,B=x.path,C=o.locals,D=[];for(n=0,v=w[n];v&&v===B[n]&&m(e,A,v.ownParams);n++,v=w[n])C=D[n]=v.locals;if(c===x&&C===x.locals)return p.transition=null,b.when(p.current);e=l(c.params,e||{});var E=a.$broadcast("$stateChangeStart",c.self,e,x.self,A);if(E.defaultPrevented)return u;for(var F=b.when(C),G=n;G<w.length;G++,v=w[G])C=D[G]=d(C),F=r(v,e,v===c,F,C);var H=p.transition=F.then(function(){var b,d,g;if(p.transition!==H)return t;for(b=B.length-1;b>=n;b--)g=B[b],g.self.onExit&&i.invoke(g.self.onExit,g.self,g.locals.globals),g.locals=null;for(b=n;b<w.length;b++)d=w[b],d.locals=D[b],d.self.onEnter&&i.invoke(d.self.onEnter,d.self,d.locals.globals);p.$current=c,p.current=c.self,p.params=e,z(p.params,k),p.transition=null;var h=c.navigable;return f.location&&h&&q.url(h.url.format(h.locals.globals.$stateParams)),a.$broadcast("$stateChangeSuccess",c.self,e,x.self,A),p.current},function(d){return p.transition!==H?t:(p.transition=null,a.$broadcast("$stateChangeError",c.self,e,x.self,A,d),b.reject(d))});return H},p.is=function(a){var b=h(a);return s(b)?p.$current===b:c},p.includes=function(a){var b=h(a);return s(b)?s(p.$current.includes[b.name]):c},p.href=function(a,b,c){c=y({lossy:!0,inherit:!1,relative:p.$current},c||{});var d=h(a,c.relative);if(!s(d))return null;b=g(k,b||{},p.$current,d);var e=d&&c.lossy?d.navigable:d,i=e&&e.url?e.url.format(l(d.params,b||{})):null;return!f.html5Mode()&&i?"#"+i:i},p.get=function(a){var b=h(a);return b&&b.self?b.self:null},p}function l(a,b){var c={};return x(a,function(a){var d=b[a];c[a]=null!=d?String(d):null}),c}function m(a,b,c){if(!c){c=[];for(var d in a)c.push(d)}for(var e=0;e<c.length;e++){var f=c[e];if(a[f]!=b[f])return!1}return!0}function n(a,b){var c={};return x(a,function(a){c[a]=b[a]}),c}var o,p,q={},r={parent:function(a){if(s(a.parent)&&a.parent)return h(a.parent);var b=/^(.+)\.[^.]+$/.exec(a.name);return b?h(b[1]):o},data:function(a){return a.parent&&a.parent.data&&(a.data=a.self.data=b.extend({},a.parent.data,a.data)),a.data},url:function(a){var b=a.url;if(u(b))return"^"==b.charAt(0)?e.compile(b.substring(1)):(a.parent.navigable||o).url.concat(b);if(e.isMatcher(b)||null==b)return b;throw new Error("Invalid url '"+b+"' in state '"+a+"'")},navigable:function(a){return a.url?a:a.parent?a.parent.navigable:null},params:function(a){if(!a.params)return a.url?a.url.parameters():a.parent.params;if(!w(a.params))throw new Error("Invalid params in state '"+a+"'");if(a.url)throw new Error("Both params and url specicified in state '"+a+"'");return a.params},views:function(a){var b={};return x(s(a.views)?a.views:{"":a},function(c,d){d.indexOf("@")<0&&(d+="@"+a.parent.name),b[d]=c}),b},ownParams:function(a){if(!a.parent)return a.params;var b={};x(a.params,function(a){b[a]=!0}),x(a.parent.params,function(c){if(!b[c])throw new Error("Missing required parameter '"+c+"' in state '"+a.name+"'");b[c]=!1});var c=[];return x(b,function(a,b){a&&c.push(b)}),c},path:function(a){return a.parent?a.parent.path.concat(a):[]},includes:function(a){var b=a.parent?y({},a.parent.includes):{};return b[a.name]=!0,b}};o=i({name:"",url:"^",views:null,"abstract":!0}),o.navigable=null,this.state=j,this.$get=k,k.$inject=["$rootScope","$q","$view","$injector","$resolve","$stateParams","$location","$urlRouter"]}function n(){function a(a,b){return{load:function(c,d){var e,f={template:null,controller:null,view:null,locals:null,notify:!0,async:!0,params:{}};return d=y(f,d),d.view&&(e=b.fromConfig(d.view,d.params,d.locals)),e&&d.notify&&a.$broadcast("$viewContentLoading",d),e}}}this.$get=a,a.$inject=["$rootScope","$templateFactory"]}function o(a,c,d,e,f){var g;try{g=e.get("$animator")}catch(h){}var i=!1,j={restrict:"ECA",terminal:!0,transclude:!0,compile:function(e,h,k){return function(e,h,l){function m(b){var g=a.$current&&a.$current.locals[p];if(g!==o){var i=t(r&&b);if(i.remove(h),n&&(n.$destroy(),n=null),!g)return o=null,v.state=null,i.restore(k(e),h);o=g,v.state=g.$$state;var j=c(i.populate(g.$template,h));if(n=e.$new(),g.$$controller){g.$scope=n;var l=d(g.$$controller,g);h.children().data("$ngControllerController",l)}j(n),n.$emit("$viewContentLoaded"),q&&n.$eval(q),f()}}var n,o,p=l[j.name]||l.name||"",q=l.onload||"",r=s(g)&&g(e,l),t=function(a){return{"true":{remove:function(a){r.leave(a.contents(),a)},restore:function(a,b){r.enter(a,b)},populate:function(a,c){var d=b.element("<div></div>").html(a).contents();return r.enter(d,c),d}},"false":{remove:function(a){a.html("")},restore:function(a,b){b.append(a)},populate:function(a,b){return b.html(a),b.contents()}}}[a.toString()]};h.append(k(e));var u=h.parent().inheritedData("$uiView");p.indexOf("@")<0&&(p=p+"@"+(u?u.state.name:""));var v={name:p,state:null};h.data("$uiView",v);var w=function(){if(!i){i=!0;try{m(!0)}catch(a){throw i=!1,a}i=!1}};e.$on("$stateChangeSuccess",w),e.$on("$viewContentLoading",w),m(!1)}}};return j}function p(a){var b=a.match(/^([^(]+?)\s*(\((.*)\))?$/);if(!b||4!==b.length)throw new Error("Invalid state ref '"+a+"'");return{state:b[1],paramExpr:b[3]||null}}function q(a){return{restrict:"A",link:function(b,c,d){var e=p(d.uiSref),f=null,g=a.$current,h="FORM"===c[0].nodeName,i=h?"action":"href",j=!0,k=c.parent().inheritedData("$uiView");k&&k.state&&k.state.name&&(g=k.state);var l=function(b){if(b&&(f=b),j){var d=a.href(e.state,f,{relative:g});return d?(c[0][i]=d,void 0):(j=!1,!1)}};e.paramExpr&&(b.$watch(e.paramExpr,function(a,b){a!==b&&l(a)},!0),f=b.$eval(e.paramExpr)),l(),h||c.bind("click",function(c){1!=c.which||c.ctrlKey||c.metaKey||c.shiftKey||(a.go(e.state,f,{relative:g}),b.$apply(),c.preventDefault())})}}}function r(a,b){function e(a){this.locals=a.locals.globals,this.params=this.locals.$stateParams}function f(){this.locals=null,this.params=null}function g(c,g){if(null!=g.redirectTo){var h,j=g.redirectTo;if(u(j))h=j;else{if(!t(j))throw new Error("Invalid 'redirectTo' in when()");h=function(a,b){return j(a,b.path(),b.search())}}b.when(c,h)}else a.state(d(g,{parent:null,name:"route:"+encodeURIComponent(c),url:c,onEnter:e,onExit:f}));return i.push(g),this}function h(a,b,d){function e(a){return""!==a.name?a:c}var f={routes:i,params:d,current:c};return b.$on("$stateChangeStart",function(a,c,d,f){b.$broadcast("$routeChangeStart",e(c),e(f))}),b.$on("$stateChangeSuccess",function(a,c,d,g){f.current=e(c),b.$broadcast("$routeChangeSuccess",e(c),e(g)),z(d,f.params)}),b.$on("$stateChangeError",function(a,c,d,f,g,h){b.$broadcast("$routeChangeError",e(c),e(f),h)}),f}var i=[];e.$inject=["$$state"],this.when=g,this.$get=h,h.$inject=["$state","$rootScope","$routeParams"]}var s=b.isDefined,t=b.isFunction,u=b.isString,v=b.isObject,w=b.isArray,x=b.forEach,y=b.extend,z=b.copy;b.module("ui.router.util",["ng"]),b.module("ui.router.router",["ui.router.util"]),b.module("ui.router.state",["ui.router.router","ui.router.util"]),b.module("ui.router",["ui.router.state"]),b.module("ui.router.compat",["ui.router"]),h.$inject=["$q","$injector"],b.module("ui.router.util").service("$resolve",h),i.$inject=["$http","$templateCache","$injector"],b.module("ui.router.util").service("$templateFactory",i),j.prototype.concat=function(a){return new j(this.sourcePath+a+this.sourceSearch)},j.prototype.toString=function(){return this.source},j.prototype.exec=function(a,b){var c=this.regexp.exec(a);if(!c)return null;var d,e=this.params,f=e.length,g=this.segments.length-1,h={};if(g!==c.length-1)throw new Error("Unbalanced capture group in route '"+this.source+"'");for(d=0;g>d;d++)h[e[d]]=c[d+1];for(;f>d;d++)h[e[d]]=b[e[d]];return h},j.prototype.parameters=function(){return this.params},j.prototype.format=function(a){var b=this.segments,c=this.params;if(!a)return b.join("");var d,e,f,g=b.length-1,h=c.length,i=b[0];for(d=0;g>d;d++)f=a[c[d]],null!=f&&(i+=encodeURIComponent(f)),i+=b[d+1];for(;h>d;d++)f=a[c[d]],null!=f&&(i+=(e?"&":"?")+c[d]+"="+encodeURIComponent(f),e=!0);return i},b.module("ui.router.util").provider("$urlMatcherFactory",k),l.$inject=["$urlMatcherFactoryProvider"],b.module("ui.router.router").provider("$urlRouter",l),m.$inject=["$urlRouterProvider","$urlMatcherFactoryProvider","$locationProvider"],b.module("ui.router.state").value("$stateParams",{}).provider("$state",m),n.$inject=[],b.module("ui.router.state").provider("$view",n),o.$inject=["$state","$compile","$controller","$injector","$anchorScroll"],b.module("ui.router.state").directive("uiView",o),q.$inject=["$state"],b.module("ui.router.state").directive("uiSref",q),r.$inject=["$stateProvider","$urlRouterProvider"],b.module("ui.router.compat").provider("$route",r).directive("ngView",o)}(window,window.angular),function(a,b,c){"use strict";b.module("ngResource",["ng"]).factory("$resource",["$http","$parse",function(a,d){function e(a){return f(a,!0).replace(/%26/gi,"&").replace(/%3D/gi,"=").replace(/%2B/gi,"+")}function f(a,b){return encodeURIComponent(a).replace(/%40/gi,"@").replace(/%3A/gi,":").replace(/%24/g,"$").replace(/%2C/gi,",").replace(/%20/g,b?"%20":"+")}function g(a,b){this.template=a+="#",this.defaults=b||{};var c=this.urlParams={};k(a.split(/\W/),function(b){b&&new RegExp("(^|[^\\\\]):"+b+"\\W").test(a)&&(c[b]=!0)}),this.template=a.replace(/\\:/g,":")}function h(d,e,f){function p(a,b){var c={};return b=l({},e,b),k(b,function(b,d){c[d]=b.charAt&&"@"==b.charAt(0)?o(a,b.substr(1)):b}),c}function q(a){m(a||{},this)}var r=new g(d);return f=l({},i,f),k(f,function(d,e){d.method=b.uppercase(d.method);var f="POST"==d.method||"PUT"==d.method||"PATCH"==d.method;q[e]=function(b,c,e,g){var h,i={},o=j,s=null;switch(arguments.length){case 4:s=g,o=e;case 3:case 2:if(!n(c)){i=b,h=c,o=e;break}if(n(b)){o=b,s=c;break}o=c,s=e;case 1:n(b)?o=b:f?h=b:i=b;break;case 0:break;default:throw"Expected between 0-4 arguments [params, data, success, error], got "+arguments.length+" arguments."}var t=this instanceof q?this:d.isArray?[]:new q(h);return a({method:d.method,url:r.url(l({},p(h,d.params||{}),i)),data:h}).then(function(a){var b=a.data;b&&(d.isArray?(t.length=0,k(b,function(a){t.push(new q(a))})):m(b,t)),(o||j)(t,a.headers)},s),t},q.prototype["$"+e]=function(a,b,d){var g,h=p(this),i=j;switch(arguments.length){case 3:h=a,i=b,g=d;break;case 2:case 1:n(a)?(i=a,g=b):(h=a,i=b||j);case 0:break;default:throw"Expected between 1-3 arguments [params, success, error], got "+arguments.length+" arguments."}var k=f?this:c;q[e].call(this,h,k,i,g)}}),q.bind=function(a){return h(d,l({},e,a),f)},q}var i={get:{method:"GET"},save:{method:"POST"},query:{method:"GET",isArray:!0},remove:{method:"DELETE"},"delete":{method:"DELETE"}},j=b.noop,k=b.forEach,l=b.extend,m=b.copy,n=b.isFunction,o=function(a,b){return d(b)(a)};return g.prototype={url:function(a){var c,d,g=this,h=this.template;a=a||{},k(this.urlParams,function(f,i){c=a.hasOwnProperty(i)?a[i]:g.defaults[i],b.isDefined(c)&&null!==c?(d=e(c),h=h.replace(new RegExp(":"+i+"(\\W)","g"),d+"$1")):h=h.replace(new RegExp("(/?):"+i+"(\\W)","g"),function(a,b,c){return"/"==c.charAt(0)?c:b+c})}),h=h.replace(/\/?#$/,"");var i=[];return k(a,function(a,b){g.urlParams[b]||i.push(f(b)+"="+f(a))}),i.sort(),h=h.replace(/\/*$/,""),h+(i.length?"?"+i.join("&"):"")}},h}])}(window,window.angular),function(a,b,c){"use strict";b.module("ngCookies",["ng"]).factory("$cookies",["$rootScope","$browser",function(a,d){function e(){var a,e,f,i;for(a in h)k(g[a])&&d.cookies(a,c);for(a in g)e=g[a],b.isString(e)?e!==h[a]&&(d.cookies(a,e),i=!0):b.isDefined(h[a])?g[a]=h[a]:delete g[a];if(i){i=!1,f=d.cookies();for(a in g)g[a]!==f[a]&&(k(f[a])?delete g[a]:g[a]=f[a],i=!0)}}var f,g={},h={},i=!1,j=b.copy,k=b.isUndefined;return d.addPollFn(function(){var b=d.cookies();f!=b&&(f=b,j(b,h),j(b,g),i&&a.$apply())})(),i=!0,a.$watch(e),g}]).factory("$cookieStore",["$cookies",function(a){return{get:function(c){var d=a[c];return d?b.fromJson(d):d},put:function(c,d){a[c]=b.toJson(d)},remove:function(b){delete a[b]}}}])}(window,window.angular),function(a,b){"use strict";function c(a){var b,c={},d=a.split(",");for(b=0;b<d.length;b++)c[d[b]]=!0;return c}function d(a,c){function d(a,d,g,h){if(d=b.lowercase(d),v[d])for(;q.last()&&w[q.last()];)f("",q.last());u[d]&&q.last()==d&&f("",d),h=r[d]||!!h,h||q.push(d);var i={};g.replace(k,function(a,b,c,d,f){var g=c||d||f||"";i[b]=e(g)}),c.start&&c.start(d,i,h)}function f(a,d){var e,f=0;if(d=b.lowercase(d))for(f=q.length-1;f>=0&&q[f]!=d;f--);if(f>=0){for(e=q.length-1;e>=f;e--)c.end&&c.end(q[e]);q.length=f}}var g,h,p,q=[],s=a;for(q.last=function(){return q[q.length-1]};a;){if(h=!0,q.last()&&x[q.last()])a=a.replace(new RegExp("(.*)<\\s*\\/\\s*"+q.last()+"[^>]*>","i"),function(a,b){return b=b.replace(n,"$1").replace(o,"$1"),c.chars&&c.chars(e(b)),""}),f("",q.last());else if(0===a.indexOf("<!--")?(g=a.indexOf("-->"),g>=0&&(c.comment&&c.comment(a.substring(4,g)),a=a.substring(g+3),h=!1)):m.test(a)?(p=a.match(j),p&&(a=a.substring(p[0].length),p[0].replace(j,f),h=!1)):l.test(a)&&(p=a.match(i),p&&(a=a.substring(p[0].length),p[0].replace(i,d),h=!1)),h){g=a.indexOf("<");var t=0>g?a:a.substring(0,g);a=0>g?"":a.substring(g),c.chars&&c.chars(e(t))}if(a==s)throw"Parse Error: "+a;s=a}f()}function e(a){return B.innerHTML=a.replace(/</g,"&lt;"),B.innerText||B.textContent||""}function f(a){return a.replace(/&/g,"&amp;").replace(q,function(a){return"&#"+a.charCodeAt(0)+";"}).replace(/</g,"&lt;").replace(/>/g,"&gt;")}function g(a){var c=!1,d=b.bind(a,a.push);return{start:function(a,e,g){a=b.lowercase(a),!c&&x[a]&&(c=a),c||1!=y[a]||(d("<"),d(a),b.forEach(e,function(a,c){var e=b.lowercase(c);1!=A[e]||z[e]===!0&&!a.match(p)||(d(" "),d(c),d('="'),d(f(a)),d('"'))}),d(g?"/>":">"))},end:function(a){a=b.lowercase(a),c||1!=y[a]||(d("</"),d(a),d(">")),a==c&&(c=!1)},chars:function(a){c||d(f(a))}}}var h=function(a){var b=[];return d(a,g(b)),b.join("")},i=/^<\s*([\w:-]+)((?:\s+[\w:-]+(?:\s*=\s*(?:(?:"[^"]*")|(?:'[^']*')|[^>\s]+))?)*)\s*(\/?)\s*>/,j=/^<\s*\/\s*([\w:-]+)[^>]*>/,k=/([\w:-]+)(?:\s*=\s*(?:(?:"((?:[^"])*)")|(?:'((?:[^'])*)')|([^>\s]+)))?/g,l=/^</,m=/^<\s*\//,n=/<!--(.*?)-->/g,o=/<!\[CDATA\[(.*?)]]>/g,p=/^((ftp|https?):\/\/|mailto:|#)/i,q=/([^\#-~| |!])/g,r=c("area,br,col,hr,img,wbr"),s=c("colgroup,dd,dt,li,p,tbody,td,tfoot,th,thead,tr"),t=c("rp,rt"),u=b.extend({},t,s),v=b.extend({},s,c("address,article,aside,blockquote,caption,center,del,dir,div,dl,figure,figcaption,footer,h1,h2,h3,h4,h5,h6,header,hgroup,hr,ins,map,menu,nav,ol,pre,script,section,table,ul")),w=b.extend({},t,c("a,abbr,acronym,b,bdi,bdo,big,br,cite,code,del,dfn,em,font,i,img,ins,kbd,label,map,mark,q,ruby,rp,rt,s,samp,small,span,strike,strong,sub,sup,time,tt,u,var")),x=c("script,style"),y=b.extend({},r,v,w,u),z=c("background,cite,href,longdesc,src,usemap"),A=b.extend({},z,c("abbr,align,alt,axis,bgcolor,border,cellpadding,cellspacing,class,clear,color,cols,colspan,compact,coords,dir,face,headers,height,hreflang,hspace,ismap,lang,language,nohref,nowrap,rel,rev,rows,rowspan,rules,scope,scrolling,shape,span,start,summary,target,title,type,valign,value,vspace,width")),B=document.createElement("pre");b.module("ngSanitize",[]).value("$sanitize",h),b.module("ngSanitize").directive("ngBindHtml",["$sanitize",function(a){return function(b,c,d){c.addClass("ng-binding").data("$binding",d.ngBindHtml),b.$watch(d.ngBindHtml,function(b){b=a(b),c.html(b||"")})}}]),b.module("ngSanitize").filter("linky",function(){var a=/((ftp|https?):\/\/|(mailto:)?[A-Za-z0-9._%+-]+@)\S*[^\s\.\;\,\(\)\{\}\<\>]/,b=/^mailto:/;return function(c){if(!c)return c;for(var d,e,f,h=c,i=[],j=g(i);d=h.match(a);)e=d[0],d[2]==d[3]&&(e="mailto:"+e),f=d.index,j.chars(h.substr(0,f)),j.start("a",{href:e}),j.chars(d[0].replace(b,"")),j.end("a"),h=h.substring(f+d[0].length);return j.chars(h),i.join("")}})}(window,window.angular);var AngularFire;angular.module("firebase",[]).value("Firebase",Firebase),angular.module("firebase").factory("angularFire",["$q","$parse","$timeout",function(a,b,c){return function(d,e,f){var g=new AngularFire(a,b,c,d);return g.associate(e,f)}}]),AngularFire=function(a,b,c,d){if(this._q=a,this._parse=b,this._timeout=c,this._initial=!0,this._remoteValue=!1,"string"==typeof d)throw new Error("Please provide a Firebase reference instead of a URL, eg: new Firebase(url)");this._fRef=d},AngularFire.prototype={associate:function(a,b){var c=this,d=this._q.defer(),e=d.promise;return this._fRef.on("value",function(e){var f=e.val(),g=angular.fromJson(angular.toJson(c._parse(b)(a)));if(c._initial){c._initial=!1;var h=!1,i=Object.prototype.toString;if(f&&i.call(g)==i.call(f))if("[object Array]"==i.call(g))h=g.concat(f),angular.equals(h,f)||(c._fRef.ref().set(h),f=h);else if("[object Object]"==i.call(g)){h=g;for(var j in f)h[j]=f[j];c._fRef.ref().update(h),f=h}null===f&&void 0!==g&&(c._fRef.ref().set(g),f=g)}var k=!1;d&&(k=d,d=!1),c._timeout(function(){c._resolve(a,b,k,f)})}),e},disassociate:function(){var a=this;a._unregister&&a._unregister(),this._fRef.off("value")},_resolve:function(a,b,c,d){var e=this;if(null===d){var f=a[b];if("object"==typeof f){var g=Object.prototype.toString;d=g.call(f)==g.call([])?[]:{}}}this._remoteValue=angular.copy(d),this._parse(b).assign(a,angular.copy(d)),c&&(c.resolve(function(){e.disassociate()}),this._watch(a,b))},_watch:function(a,b){var c=this;c._unregister=a.$watch(b,function(){if(!c._initial){var d=angular.fromJson(angular.toJson(c._parse(b)(a)));if(!angular.equals(d,c._remoteValue)){var e=Object.prototype.toString;"[object Object]"==e.call(d)?c._fRef.set?c._fRef.set(d):c._fRef.ref().update(d):c._fRef.ref().set(d)}}},!0),a.$on("$destroy",function(){c.disassociate()})},_log:function(a){console&&console.log&&console.log(a)}},angular.module("firebase").factory("angularFireCollection",["$timeout",function(a){return function(b,c){function d(a){return a?l[a]+1:0}function e(a,b){l[b.$id]=a,m.splice(a,0,b)}function f(a){var b=l[a];m.splice(b,1),l[a]=void 0}function g(a,b){m[a]=b}function h(a,b,c){m.splice(a,1),m.splice(b,0,c),i(a,b)}function i(a,b){var c=m.length;b=b||c,b>c&&(b=c);for(var d=a;b>d;d++){var e=m[d];e.$index=l[e.$id]=d}}if("string"==typeof b)throw new Error("Please provide a Firebase reference instead of a URL, eg: new Firebase(url)");var j=function(a,b){this.$ref=a.ref(),this.$id=a.name(),this.$index=b,angular.extend(this,{$priority:a.getPriority()},a.val())},k=[function(a){return null==a.$priority?0:angular.isNumber(a.$priority)?1:angular.isString(a.$priority)?2:void 0},function(a){return a.$priority?a.$priority:1/0},function(a){return a.$id}],l={},m=[];return c&&"function"==typeof c&&b.once("value",c),b.on("child_added",function(b,c){a(function(){var a=d(c);e(a,new j(b,a)),i(a)})}),b.on("child_removed",function(b){a(function(){var a=b.name(),c=l[a];f(a),i(c)})}),b.on("child_changed",function(b,c){a(function(){var a=l[b.name()],e=d(c),f=new j(b,a);g(a,f),e!==a&&h(a,e,f)})}),b.on("child_moved",function(b,c){a(function(){var a=l[b.name()],e=d(c),f=m[a];h(a,e,f)})}),m.getByName=function(a){return m[l[a]]},m.getByNames=function(a){for(var b=[],c=0,d=a.length;d>c;c++)b.push(m[l[a[c]]]);return b},m.add=function(a,c){var d,e=angular.fromJson(angular.toJson(a));return d=c?b.ref().push(e,c):b.ref().push(e)},m.remove=function(a,b){var c=angular.isString(a)?m[l[a]]:a;b?c.$ref.remove(b):c.$ref.remove()},m.update=function(a,b){var c=angular.isString(a)?m[l[a]]:a,d=angular.fromJson(angular.toJson(c));b?c.$ref.update(d,b):c.$ref.update(d)},m.set=function(a,b){var c=angular.isString(a)?m[l[a]]:a,d=angular.fromJson(angular.toJson(c));b?c.$ref.set(d,b):c.$ref.set(d)},m.order=k,m}}]),angular.module("firebase").factory("angularFireAuth",["$rootScope","$parse","$timeout","$location","$route","$q",function(a,b,c,d,e,f){function g(a){var b=a.split(".");if(!b instanceof Array||3!==b.length)throw new Error("Invalid JWT");var c=b[1];return window.atob?JSON.parse(decodeURIComponent(escape(window.atob(c)))):a}function h(a,d,e,f){d&&c(function(){b(d).assign(a,e),f()})}function i(a,b,c){a.authRequired&&!c._authenticated&&(c._redirectTo=void 0===a.pathTo?d.path():a.pathTo===b?"/":a.pathTo,d.replace(),d.path(b))}return{initialize:function(b,c){var d=this;if("string"==typeof b)throw new Error("Please provide a Firebase reference instead of a URL, eg: new Firebase(url)");if(c=c||{},this._scope=a,!c.scope)throw new Error("Scope not provided to angularFireAuth!");if(this._scope=c.scope,!c.name)throw new Error("Model name not provided to angularFireAuth!");if(this._name=c.name,this._cb=function(){},c.callback&&"function"==typeof c.callback&&(this._cb=c.callback),this._redirectTo=null,this._authenticated=!1,c.path&&(e.current&&i(e.current,c.path,d),a.$on("$routeChangeStart",function(a,b){i(b,c.path,d)})),this._ref=b,c.simple===!1)return h(this._scope,this._name,null,function(){}),void 0;if(!window.FirebaseSimpleLogin){var f=new Error("FirebaseSimpleLogin undefined, did you include firebase-simple-login.js?");return a.$broadcast("angularFireAuth:error",f),void 0}var g=new FirebaseSimpleLogin(this._ref,function(b,c){d._cb(b,c),b?a.$broadcast("angularFireAuth:error",b):c?d._loggedIn(c):d._loggedOut()});this._authClient=g},login:function(b,c){var d=this._watchForLogin();switch(b){case"github":case"persona":case"twitter":case"facebook":case"password":if(this._authClient)this._authClient.login(b,c);else{var e=new Error("Simple Login not initialized");a.$broadcast("angularFireAuth:error",e)}break;default:var f,h=this;try{f=g(b),this._ref.auth(b,function(b){b?a.$broadcast("angularFireAuth:error",b):h._loggedIn(f)})}catch(i){a.$broadcast("angularFireAuth:error",i)}}return d},createUser:function(b,d,e,f){var g=this;this._authClient.createUser(b,d,function(h,i){try{h?a.$broadcast("angularFireAuth:error",h):f||g.login("password",{email:b,password:d})}catch(j){a.$broadcast("angularFireAuth:error",j)}e&&c(function(){e(h,i)})})},logout:function(){this._authClient?this._authClient.logout():(this._ref.unauth(),this._loggedOut())},_loggedIn:function(b){var c=this;this._authenticated=!0,h(this._scope,this._name,b,function(){a.$broadcast("angularFireAuth:login",b),c._redirectTo&&(d.replace(),d.path(c._redirectTo),c._redirectTo=null)})},_loggedOut:function(){this._authenticated=!1,h(this._scope,this._name,null,function(){a.$broadcast("angularFireAuth:logout")})},_watchForLogin:function(){function b(a,b){c(function(){a?e.reject(a):e.resolve(b)});for(var f=0;f<d.length;f++)d[f]()}var d=[],e=f.defer();return d.push(a.$on("angularFireAuth:login",function(a,c){b(null,c)})),d.push(a.$on("angularFireAuth:error",function(a,c){b(c,null)})),e.promise}}}]);