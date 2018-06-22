/* eslint-disable no-undef */

import React, { Component } from 'react'
import PropTypes from 'prop-types'
import { connect } from 'react-redux'
import { withRouter } from 'react-router-dom'
import { loadUsers } from '../actions'

class UserListPage extends Component {
  static propTypes = {
    users: PropTypes.object,
    loadUsers: PropTypes.func.isRequired
  }

  componentWillMount() {
    loadUsers()
  }

  componentWillReceiveProps(nextProps) {
  }

  render() {
    const { users } = this.props
    return (
      <div>
      </div>
    )
  }
}

const mapStateToProps = (state, ownProps) => {
  const {
    entities: { users }
  } = state

  return {
    users: users
  }
}

export default withRouter(connect(mapStateToProps, {
  loadUsers
})(UserListPage))