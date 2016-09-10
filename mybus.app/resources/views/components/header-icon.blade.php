<nav class="navbar navbar-default navbar-static-top" id="topNav">
  <div class="col-xs-3 icon">
    <a href="{{ url('/') }}">
      <span class="glyphicon glyphicon-home"></span>
    </a>
  </div>
  <div class="col-xs-3 icon">
    <a href="#">
      <span class="glyphicon glyphicon-search"></span>
    </a>
  </div>
  <div class="col-xs-3 icon">
    @if (Auth::guest())
      <a href="#">
        <span class="glyphicon glyphicon-user"></span>
      </a>
    @endif
  </div>
  <div class="col-xs-3 icon">
    @if (Auth::guest())
      <a href="{{ url('/login') }}">
        <span class="glyphicon glyphicon-log-in"></span>
      </a>
    @else
      <a href="{{ url('/logout') }}">
          <span class="glyphicon glyphicon-log-out"></span>
      </a>
    @endif
  </div>
</nav>
