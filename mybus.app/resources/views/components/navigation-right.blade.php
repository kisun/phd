{{--
  The header will be a right-slideout on mobile,
  and a right-panel on large+
--}}

<nav id="rightNav">
  <a href="#" class="nav-toggle">
    @for($i=0;$i<3;$i++)
      <span class="nav-toggle-bar"></span>
      <span class="nav-toggle-bar"></span>
      <span class="nav-toggle-bar"></span>
    @endfor
  </a>

  <ul class="list-unstyled">
    <li><a href="{{ url('/') }}">Home</a></li>
    <hr>
    @if (Auth::guest())
      <li><a href="{{ url('/login') }}">
        {{-- <span class="glyphicon glyphicon-log-in"></span> --}}
        Log in</a></li>
      <li><a href="{{ url('/register') }}">
        Register</a></li>
    @else
      <li><a href="{{ url('/logout') }}">
        {{-- <span class="glyphicon glyphicon-log-out"></span> --}}
        Logout</a></li>
    @endif
  </ul>
</nav>
