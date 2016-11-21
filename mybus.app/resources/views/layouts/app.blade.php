<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1, minimal-ui">
    <meta name="apple-mobile-web-app-capable" content="yes">
    <meta name="apple-mobile-web-app-status-bar-style" content="blue">


    <!-- CSRF Token -->
    <meta name="csrf-token" content="{{ csrf_token() }}">

    <title>{{ config('app.name', 'Laravel') }}</title>

    <!-- Styles -->
    <link href="/css/app.css" rel="stylesheet">

    <!-- Scripts -->
    <script>
        window.Laravel = <?php echo json_encode([
            'csrfToken' => csrf_token(),
        ]); ?>
    </script>
</head>
<body>
  <div class="container-fluid @yield('container-class', 'fullpage-app')">
    <div class="row fullpage-row">
      {{-- @if (Session::has('status'))
        <alert msg="{{ Session::get('status') }}"></alert>
      @endif --}}

      <div class="col-lg-3 col-lg-push-9 app-nav app-nav-right">
        @include('components.navigation-right')
      </div>

      <div class="col-lg-9 col-lg-pull-3 app-content">
        @yield('content')
      </div>
    </div>
  </div>

    <!-- Scripts -->
    <script src="/js/app.js"></script>

    @yield('endmatter')
</body>
</html>
