@extends('layouts.app')

@section('container-class', 'scrollpage-app')

@section('content')

  <div class="container-fluid">
    <div class="row">
      <div class="page-header">
        <h3>List of Symbols</h3>
      </div>

      <p class="small">
        \( i = 1, \ldots, N \) is the particle index<br>
        \( j = 1, \ldots, M \) is the stop/sequence index for a given route<br>
        \( k = 0, 1, \ldots \) is the observation counter
        (note that 0 indicates intial or prior value, e.g., \( v_0 \sim \mathcal{U}(0,30) \))<br>
        \( \ell \) identifies unique stops
      </p>

      <div class="col-md-4">
        <h4>Parameters</h4>

        <table class="table table-condensed">
          <tbody>
            <tr>
              <td>\( \eta_\ell \)</td>
              <td>probabiltiy of any bus stopping at stop \( \ell \)</td>
            </tr>

            <tr>
              <td>\( \tau_\ell \)</td>
              <td>average dwell time at stop \( \ell \) (\(s\))</td>
            </tr>

            <tr>
              <td>\( \gamma \)</td>
              <td>minimum dwell time (\(s\))</td>
            </tr>

            <tr>
              <td>\( \sigma_y \)</td>
              <td>observation (GPS) error</td>
            </tr>

            <tr>
              <td>\( \pi_{\ell|r} \)</td>
              <td>average probability of trip \( r \) stopping at stop \( \ell \)</td>
            </tr>

            <tr>
              <td>\( \tau_{\ell|r} \)</td>
              <td>average dwell time of trip \( r \) at stop \( \ell \), conditional on stopping (\(s\))</td>
            </tr>
          </tbody>
        </table>
      </div>

      <div class="col-md-4">
        <h4>Variables*</h4>

        <p class="small">Random variables have a <span class="bg-danger">pink background</span>.</p>

        <table class="table table-condensed">
          <tbody>
            <tr>
              <td>\(\mathbf{X}_k\)</td>
              <td>the state vector, at time \(k\)<br>
                \( \mathbf{X}_k = \begin{bmatrix} d_k & v_k & s_k & A_{s_k} & D_{s_k} \end{bmatrix}^T \)
              </td>
            </tr>

            <tr>
              <td>\(d_k\)</td>
              <td>distance into trip (\( m \))</td>
            </tr>

            <tr class="danger">
              <td>\(v_k\)</td>
              <td>velocity (speed) of the vehicle (\( m s^{-1} \))</td>
            </tr>

            <tr>
              <td>\(s_k\)</td>
              <td>route segment/stop sequence</td>
            </tr>

            <tr>
              <td>\( A_j \)</td>
              <td>Arrival time at stop \(j\) (UNIX timestamp)</td>
            </tr>

            <tr>
              <td>\( D_j \)</td>
              <td>Departure time from stop \(j\) (UNIX timestamp)</td>
            </tr>

            <tr>
              <td>\( \tilde A_j \)</td>
              <td>Arrival time delay at stop \(j\) (\(s\))</td>
            </tr>

            <tr>
              <td>\( \tilde D_j \)</td>
              <td>Departure time delay from stop \(j\) (\(s\))</td>
            </tr>

            <tr>
              <td>\( \delta_k \)</td>
              <td>Elapsed time since the previous observation (\( t_k - t_{k-1} \)) (\(s\))</td>
            </tr>

            <tr class="danger">
              <td>\( p_j \)</td>
              <td>binary indicator that the vehicle stopped at stop \( j \)</td>
            </tr>

            <tr class="danger">
              <td>\( \bar t_j \)</td>
              <td>dwell time at stop \( j \) (\(s\))</td>
            </tr>

            {{-- <tr>
              <td>\(  \)</td>
              <td></td>
            </tr> --}}

          </tbody>
        </table>

        <p class="small">
          In the particle filter, each of these variables is represented by a sample of size \(N\),
          with superscripts \( (i) \), e.g., \( d_k^{(i)} \).
        </p>
      </div>

      <div class="col-md-4">
        <h4>Data*</h4>

        <table class="table table-condensed">
          <tbody>
            <tr>
              <td>\( \mathbf{Y}_k \)</td>
              <td>vehicle position, observation \(k\)<br>
                \( \mathbf{Y}_k = \begin{bmatrix} \phi_k & \lambda_k & t_k \end{bmatrix}^T \)
              </td>
            </tr>

            <tr>
              <td>\( \phi_k \)</td>
              <td>latitude of vehicle (degrees)</td>
            </tr>

            <tr>
              <td>\( \lambda_k \)</td>
              <td>longitude of vehicle (degrees)</td>
            </tr>

            <tr>
              <td>\( t_k \)</td>
              <td>(UNIX) timestamp of observation \( k \)</td>
            </tr>

            <tr>
              <td>\( T^a_j \)</td>
              <td>arrival time (UNIX timestamp) at stop \( j \)</td>
            </tr>

            <tr>
              <td>\( T^d_j \)</td>
              <td>departure time (UNIX timestamp) at stop \( j \)</td>
            </tr>

            <tr>
              <td>\( \tilde T^a_j \)</td>
              <td>arrival delay (\( s \)) at stop \( j \)</td>
            </tr>

            <tr>
              <td>\( {\tilde T}^d_j \)</td>
              <td>departure time delay (\( s \)) at stop \( j \)</td>
            </tr>

            <tr>
              <td>\( \mathbf{S}^t \)</td>
              <td>scheduled arrival** times at route's stops<br>
                \( \mathbf{S}^t = \{ S^t_j : j = 1, \ldots, M \} \)
              </td>

            <tr>
              <td>\( \mathbf{S}^d \)</td>
              <td>distance of stop \(j\) into route<br>
                \( \mathbf{S}^d = \{ S^d_j : j = 1, \ldots, M \} \)
              </td>
            </tr>
          </tbody>
        </table>
      </div>
    </div>

    <hr>
    <h5>Footnotes</h5>

    <p class="small">
      <ul class="list-unstyled">
        <li><strong>*</strong> for a single given vehicle</li>
        <li><strong>**</strong> For the first stop (\(j=1\)) and any layover stops, \(S_j\) is the departure time</li>
      </ul>
    </p>
  </div>

@endsection


@section('endmatter')
  <script type="text/javascript" async
    src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML">
  </script>
@endsection
