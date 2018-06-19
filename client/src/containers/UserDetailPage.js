/* eslint-disable no-undef */

import React, { Component } from 'react'
import PropTypes from 'prop-types'
import { connect } from 'react-redux'
import { withRouter } from 'react-router-dom'
import { loadUser } from '../actions'
import User from '../components/User'

const loadData = ({ login, loadUser, loadStarred }) => {
  loadUser(login, [ 'name' ])
}

class UserDetailPage extends Component {
  static propTypes = {
    login: PropTypes.string.isRequired,
    user: PropTypes.object,
    loadUser: PropTypes.func.isRequired
  }

  componentWillMount() {
    loadData(this.props)
  }

  componentWillReceiveProps(nextProps) {
    if (nextProps.login !== this.props.login) {
      loadData(nextProps)
    }
  }

  render() {
    const { user, login } = this.props
    if (!user) {
      return <h1><i>Loading {login}{"'s profile..."}</i></h1>
    }

    return (
      <div>
        <User user={user} />
      </div>
    )
  }
}

const mapStateToProps = (state, ownProps) => {
  const login = ownProps.match.params.login.toLowerCase()

  const {
    entities: { users }
  } = state

  return {
    login,
    user: users[login]
  }
}

export default withRouter(connect(mapStateToProps, {
  loadUser
})(UserDetailPage))
