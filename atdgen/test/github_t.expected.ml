(* Auto-generated from "github.atd" *)
              [@@@ocaml.warning "-27-32-35-39"]

type update_gist = {
  update_gist_description (*atd description *): string;
  update_gist_files (*atd files *): (string * update_gist) list
}

type json = Yojson.Safe.json

type wiki_page_action = [
    `Created
  | `Edited
  | `Unknown of (string * json option)
]

type wiki_page = {
  wiki_page_name (*atd name *): string;
  wiki_page_title (*atd title *): string;
  wiki_page_action (*atd action *): wiki_page_action;
  wiki_page_sha (*atd sha *): string;
  wiki_page_html_url (*atd html_url *): string
}

type bool_as_string = bool

type web_hook_config = {
  web_hook_config_url (*atd url *): string;
  web_hook_config_content_type (*atd content_type *): string option;
  web_hook_config_insecure_ssl (*atd insecure_ssl *): bool_as_string;
  web_hook_config_secret (*atd secret *): string option
}

type watch_action = [ `Started | `Unknown of (string * json option) ]

type watch_event = { watch_event_action (*atd action *): watch_action }

type user_info = {
  user_info_name (*atd name *): string option;
  user_info_company (*atd company *): string option;
  user_info_blog (*atd blog *): string option;
  user_info_location (*atd location *): string option;
  user_info_email (*atd email *): string option;
  user_info_hireable (*atd hireable *): bool;
  user_info_bio (*atd bio *): string;
  user_info_public_repos (*atd public_repos *): int;
  user_info_public_gists (*atd public_gists *): int;
  user_info_followers (*atd followers *): int;
  user_info_following (*atd following *): int;
  user_info_created_at (*atd created_at *): string;
  user_info_updated_at (*atd updated_at *): string;
  user_info_html_url (*atd html_url *): string;
  user_info_login (*atd login *): string;
  user_info_id (*atd id *): Int64.t;
  user_info_url (*atd url *): string;
  user_info_avatar_url (*atd avatar_url *): string option
}

type user = {
  user_login (*atd login *): string;
  user_id (*atd id *): Int64.t;
  user_url (*atd url *): string;
  user_avatar_url (*atd avatar_url *): string option
}

type update_release = {
  update_release_tag_name (*atd tag_name *): string option;
  update_release_target_commitish (*atd target_commitish *): string option;
  update_release_name (*atd name *): string option;
  update_release_body (*atd body *): string option;
  update_release_draft (*atd draft *): bool option;
  update_release_prerelease (*atd prerelease *): bool option
}

type state = [ `Open | `Closed ]

type update_pull = {
  update_pull_title (*atd title *): string option;
  update_pull_body (*atd body *): string option;
  update_pull_state (*atd state *): state option;
  update_pull_base (*atd base *): string option
}

type update_milestone = {
  update_milestone_title (*atd title *): string option;
  update_milestone_state (*atd state *): state option;
  update_milestone_description (*atd description *): string option;
  update_milestone_due_on (*atd due_on *): string option
}

type update_issue = {
  update_issue_title (*atd title *): string option;
  update_issue_body (*atd body *): string option;
  update_issue_state (*atd state *): state option;
  update_issue_assignee (*atd assignee *): string option;
  update_issue_milestone (*atd milestone *): int option;
  update_issue_labels (*atd labels *): string list option
}

type hook_config = [
    `Web of web_hook_config
  | `Unknown of (string * json option)
]

type event_type = [
    `CommitComment
  | `Create
  | `Delete
  | `Deployment
  | `DeploymentStatus
  | `Download
  | `Follow
  | `Fork
  | `ForkApply
  | `Gist
  | `Gollum
  | `IssueComment
  | `Issues
  | `Member
  | `PageBuild
  | `Public
  | `PullRequest
  | `PullRequestReviewComment
  | `Push
  | `Release
  | `Repository
  | `Status
  | `TeamAdd
  | `Watch
  | `All
  | `Unknown of (string * json option)
]

type update_hook = {
  update_hook_config (*atd config *): hook_config;
  update_hook_events (*atd events *): event_type list option;
  update_hook_active (*atd active *): bool
}

type update_gist_file = {
  update_gist_file_content (*atd content *): string option;
  update_gist_file_name (*atd name *): string option
}

type pull_ref = {
  pull_ref_url (*atd url *): string;
  pull_ref_html_url (*atd html_url *): string;
  pull_ref_diff_url (*atd diff_url *): string;
  pull_ref_patch_url (*atd patch_url *): string
}

type milestone = {
  milestone_url (*atd url *): string;
  milestone_number (*atd number *): int;
  milestone_state (*atd state *): state;
  milestone_title (*atd title *): string;
  milestone_description (*atd description *): string;
  milestone_creator (*atd creator *): user option;
  milestone_open_issues (*atd open_issues *): int;
  milestone_closed_issues (*atd closed_issues *): int;
  milestone_created_at (*atd created_at *): string;
  milestone_due_on (*atd due_on *): string option
}

type label = {
  label_url (*atd url *): string;
  label_name (*atd name *): string;
  label_color (*atd color *): string
}

type issue_sort = [
    `Created
  | `Updated
  | `Comments
  | `Unknown of (string * json option)
]

type direction = [ `Asc | `Desc ]

type issue = {
  issue_url (*atd url *): string;
  issue_html_url (*atd html_url *): string;
  issue_number (*atd number *): int;
  issue_state (*atd state *): state;
  issue_title (*atd title *): string;
  issue_body (*atd body *): string;
  issue_user (*atd user *): user;
  issue_labels (*atd labels *): label list;
  issue_comments (*atd comments *): int;
  issue_created_at (*atd created_at *): string;
  issue_updated_at (*atd updated_at *): string;
  issue_closed_at (*atd closed_at *): string option;
  issue_milestone (*atd milestone *): milestone option;
  issue_sort (*atd sort *): issue_sort;
  issue_direction (*atd direction *): direction;
  issue_mentioned (*atd mentioned *): string list option;
  issue_pull_request (*atd pull_request *): pull_ref option
}

type timeline_source = {
  timeline_source_id (*atd id *): int option;
  timeline_source_url (*atd url *): string option;
  timeline_source_actor (*atd actor *): user option;
  timeline_source_issue (*atd issue *): issue option
}

type timeline_action = [
    `Assigned | `Closed | `Commented | `Committed | `Cross_referenced
  | `Demilestoned | `Head_ref_deleted | `Head_ref_restored | `Labeled
  | `Locked | `Mentioned | `Merged | `Milestoned | `Referenced | `Renamed
  | `Reopened | `Review_dismissed | `Review_requested
  | `Review_request_removed | `Subscribed | `Unassigned | `Unlabeled
  | `Unlocked | `Unsubscribed
]

type issue_rename = {
  issue_rename_from (*atd from *): string;
  issue_rename_to (*atd to *): string
}

type base_label = {
  base_label_name (*atd name *): string;
  base_label_color (*atd color *): string
}

type timeline_event = {
  timeline_event_id (*atd id *): int option;
  timeline_event_url (*atd url *): string option;
  timeline_event_actor (*atd actor *): user option;
  timeline_event_commit_id (*atd commit_id *): string option;
  timeline_event_event (*atd event *): timeline_action;
  timeline_event_created_at (*atd created_at *): string;
  timeline_event_label (*atd label *): base_label option;
  timeline_event_assignee (*atd assignee *): user option;
  timeline_event_milestone (*atd milestone *): milestone option;
  timeline_event_source (*atd source *): timeline_source option;
  timeline_event_rename (*atd rename *): issue_rename option
}

type timeline_events = timeline_event list

type change = { change_from (*atd from *): string }

type ticket_changes = {
  ticket_changes_title (*atd title *): change option;
  ticket_changes_body (*atd body *): change option
}

type team = {
  team_url (*atd url *): string;
  team_name (*atd name *): string;
  team_id (*atd id *): Int64.t
}

type teams = team list

type team_permission = [
    `Pull
  | `Push
  | `Admin
  | `Unknown of (string * json option)
]

type org = {
  org_login (*atd login *): string;
  org_id (*atd id *): Int64.t;
  org_url (*atd url *): string;
  org_avatar_url (*atd avatar_url *): string option
}

type team_info = {
  team_info_permission (*atd permission *): team_permission;
  team_info_members_count (*atd members_count *): int;
  team_info_repos_count (*atd repos_count *): int;
  team_info_organization (*atd organization *): org;
  team_info_url (*atd url *): string;
  team_info_name (*atd name *): string;
  team_info_id (*atd id *): Int64.t
}

type team_infos = team_info list

type team_add_info = {
  team_add_info_slug (*atd slug *): string;
  team_add_info_permission (*atd permission *): team_permission;
  team_add_info_members_url (*atd members_url *): string;
  team_add_info_repositories_url (*atd repositories_url *): string;
  team_add_info_url (*atd url *): string;
  team_add_info_name (*atd name *): string;
  team_add_info_id (*atd id *): Int64.t
}

type repository = {
  repository_owner (*atd owner *): user;
  repository_full_name (*atd full_name *): string;
  repository_description (*atd description *): string option;
  repository_private (*atd private *): bool;
  repository_fork (*atd fork *): bool;
  repository_html_url (*atd html_url *): string;
  repository_clone_url (*atd clone_url *): string;
  repository_git_url (*atd git_url *): string;
  repository_ssh_url (*atd ssh_url *): string;
  repository_svn_url (*atd svn_url *): string;
  repository_mirror_url (*atd mirror_url *): string option;
  repository_homepage (*atd homepage *): string;
  repository_language (*atd language *): string option;
  repository_forks_count (*atd forks_count *): int;
  repository_subscribers_count (*atd subscribers_count *): int option;
  repository_stargazers_count (*atd stargazers_count *): int;
  repository_size (*atd size *): int;
  repository_default_branch (*atd default_branch *): string option;
  repository_open_issues_count (*atd open_issues_count *): int;
  repository_pushed_at (*atd pushed_at *): string option;
  repository_created_at (*atd created_at *): string;
  repository_updated_at (*atd updated_at *): string;
  repository_organization (*atd organization *): user option;
  repository_has_issues (*atd has_issues *): bool;
  repository_has_wiki (*atd has_wiki *): bool;
  repository_has_downloads (*atd has_downloads *): bool;
  repository_has_pages (*atd has_pages *): bool;
  repository_id (*atd id *): Int64.t;
  repository_name (*atd name *): string;
  repository_url (*atd url *): string
}

type team_add_event = {
  team_add_event_team (*atd team *): team_add_info option;
  team_add_event_user (*atd user *): user option;
  team_add_event_repository (*atd repository *): repository option;
  team_add_event_organization (*atd organization *): org
}

type obj_type = [ `Blob | `Tree | `Commit | `Tag ]

type obj = {
  obj_ty (*atd ty *): obj_type;
  obj_sha (*atd sha *): string;
  obj_url (*atd url *): string
}

type info = {
  info_date (*atd date *): string;
  info_email (*atd email *): string;
  info_name (*atd name *): string
}

type tag = {
  tag_obj (*atd obj *): obj;
  tag_url (*atd url *): string;
  tag_sha (*atd sha *): string;
  tag_tag (*atd tag *): string;
  tag_message (*atd message *): string;
  tag_tagger (*atd tagger *): info
}

type status_state = [
    `Pending
  | `Success
  | `Failure
  | `Error
  | `Unknown of (string * json option)
]

type status = {
  status_creator (*atd creator *): user;
  status_url (*atd url *): string;
  status_updated_at (*atd updated_at *): string;
  status_created_at (*atd created_at *): string;
  status_id (*atd id *): Int64.t;
  status_state (*atd state *): status_state;
  status_target_url (*atd target_url *): string option;
  status_description (*atd description *): string option;
  status_context (*atd context *): string option
}

type statuses = status list

type status_branch_commit = {
  status_branch_commit_sha (*atd sha *): string;
  status_branch_commit_url (*atd url *): string
}

type status_branch = {
  status_branch_name (*atd name *): string;
  status_branch_commit (*atd commit *): status_branch_commit
}

type git_commit = {
  git_commit_url (*atd url *): string;
  git_commit_author (*atd author *): info;
  git_commit_message (*atd message *): string
}

type commit = {
  commit_url (*atd url *): string;
  commit_sha (*atd sha *): string;
  commit_git (*atd git *): git_commit;
  commit_author (*atd author *): user option;
  commit_committer (*atd committer *): user option
}

type status_event = {
  status_event_sha (*atd sha *): string;
  status_event_target_url (*atd target_url *): string option;
  status_event_context (*atd context *): string option;
  status_event_description (*atd description *): string option;
  status_event_state (*atd state *): status_state;
  status_event_commit (*atd commit *): commit;
  status_event_branches (*atd branches *): status_branch list
}

type scope = [
    `User
  | `User_email
  | `User_follow
  | `Public_repo
  | `Repo
  | `Repo_deployment
  | `Repo_status
  | `Delete_repo
  | `Notifications
  | `Gist
  | `Read_repo_hook
  | `Write_repo_hook
  | `Admin_repo_hook
  | `Admin_org_hook
  | `Read_org
  | `Write_org
  | `Admin_org
  | `Read_public_key
  | `Write_public_key
  | `Admin_public_key
  | `Unknown of (string * json option)
]

type repositories = repository list

type repository_search = {
  repository_search_total_count (*atd total_count *): int;
  repository_search_incomplete_results (*atd incomplete_results *): bool;
  repository_search_items (*atd items *): repositories
}

type repository_action = [
    `Created
  | `Deleted
  | `Publicized
  | `Privatized
  | `Unknown of (string * json option)
]

type repository_event = {
  repository_event_action (*atd action *): repository_action;
  repository_event_repository (*atd repository *): repository
}

type repo_commit = {
  repo_commit_sha (*atd sha *): string;
  repo_commit_url (*atd url *): string
}

type repo_tag = {
  repo_tag_name (*atd name *): string;
  repo_tag_commit (*atd commit *): repo_commit;
  repo_tag_zipball_url (*atd zipball_url *): string;
  repo_tag_tarball_url (*atd tarball_url *): string
}

type repo_tags = repo_tag list

type repo_issues_action = [
    `Closed
  | `Reopened
  | `Subscribed
  | `Merged
  | `Referenced
  | `Mentioned
  | `Assigned
  | `Unassigned
  | `Labeled
  | `Unlabeled
  | `Milestoned
  | `Demilestoned
  | `Renamed
  | `Locked
  | `Unlocked
  | `Head_ref_deleted
  | `Head_ref_restored
  | `Unknown of (string * json option)
]

type linked_user = {
  linked_user_html_url (*atd html_url *): string;
  linked_user_login (*atd login *): string;
  linked_user_id (*atd id *): Int64.t;
  linked_user_url (*atd url *): string;
  linked_user_avatar_url (*atd avatar_url *): string option
}

type repo_issues_event = {
  repo_issues_event_issue (*atd issue *): issue;
  repo_issues_event_id (*atd id *): int;
  repo_issues_event_url (*atd url *): string;
  repo_issues_event_actor (*atd actor *): linked_user option;
  repo_issues_event_event (*atd event *): repo_issues_action;
  repo_issues_event_created_at (*atd created_at *): string;
  repo_issues_event_label (*atd label *): base_label option;
  repo_issues_event_assignee (*atd assignee *): linked_user option;
  repo_issues_event_assigner (*atd assigner *): linked_user option;
  repo_issues_event_milestone (*atd milestone *): milestone option;
  repo_issues_event_rename (*atd rename *): issue_rename option
}

type repo_issues_events = repo_issues_event list

type repo_issue_event = {
  repo_issue_event_id (*atd id *): int;
  repo_issue_event_url (*atd url *): string;
  repo_issue_event_actor (*atd actor *): linked_user option;
  repo_issue_event_event (*atd event *): repo_issues_action;
  repo_issue_event_created_at (*atd created_at *): string;
  repo_issue_event_label (*atd label *): base_label option;
  repo_issue_event_assignee (*atd assignee *): linked_user option;
  repo_issue_event_assigner (*atd assigner *): linked_user option;
  repo_issue_event_milestone (*atd milestone *): milestone option;
  repo_issue_event_rename (*atd rename *): issue_rename option
}

type repo_issue_events = repo_issue_event list

type repo_branch = {
  repo_branch_name (*atd name *): string;
  repo_branch_commit (*atd commit *): repo_commit
}

type repo_branches = repo_branch list

type repo = {
  repo_id (*atd id *): Int64.t;
  repo_name (*atd name *): string;
  repo_url (*atd url *): string
}

type release = {
  release_id (*atd id *): Int64.t;
  release_tag_name (*atd tag_name *): string;
  release_target_commitish (*atd target_commitish *): string option;
  release_name (*atd name *): string option;
  release_body (*atd body *): string option;
  release_draft (*atd draft *): bool;
  release_prerelease (*atd prerelease *): bool;
  release_created_at (*atd created_at *): string;
  release_published_at (*atd published_at *): string;
  release_url (*atd url *): string;
  release_html_url (*atd html_url *): string;
  release_assets_url (*atd assets_url *): string;
  release_upload_url (*atd upload_url *): string
}

type releases = release list

type release_repo = {
  release_repo_user (*atd user *): string;
  release_repo_repo (*atd repo *): string;
  release_repo_release (*atd release *): release
}

type release_repos = release_repo list

type release_action = [ `Published | `Unknown of (string * json option) ]

type release_event = {
  release_event_action (*atd action *): release_action;
  release_event_release (*atd release *): release
}

type ref = [ `Repository | `Branch of string | `Tag of string ]

type rate = {
  rate_limit (*atd limit *): int;
  rate_remaining (*atd remaining *): int;
  rate_reset (*atd reset *): float
}

type rate_resources = {
  rate_resources_core (*atd core *): rate;
  rate_resources_search (*atd search *): rate
}

type rate_limit = { rate_limit_resources (*atd resources *): rate_resources }

type push_event_author = {
  push_event_author_name (*atd name *): string;
  push_event_author_email (*atd email *): string
}

type push_event_hook_commit = {
  push_event_hook_commit_id (*atd id *): string;
  push_event_hook_commit_tree_id (*atd tree_id *): string;
  push_event_hook_commit_url (*atd url *): string;
  push_event_hook_commit_message (*atd message *): string;
  push_event_hook_commit_author (*atd author *): push_event_author;
  push_event_hook_commit_distinct (*atd distinct *): bool
}

type push_event_hook = {
  push_event_hook_after (*atd after *): string;
  push_event_hook_created (*atd created *): bool;
  push_event_hook_deleted (*atd deleted *): bool;
  push_event_hook_forced (*atd forced *): bool;
  push_event_hook_commits (*atd commits *): push_event_hook_commit list;
  push_event_hook_head_commit (*atd head_commit *):
    push_event_hook_commit option;
  push_event_hook_ref (*atd ref *): string;
  push_event_hook_before (*atd before *): string
}

type push_event_commit_base = {
  push_event_commit_base_url (*atd url *): string;
  push_event_commit_base_message (*atd message *): string;
  push_event_commit_base_author (*atd author *): push_event_author;
  push_event_commit_base_distinct (*atd distinct *): bool
}

type push_event_commit = {
  push_event_commit_sha (*atd sha *): string;
  push_event_commit_url (*atd url *): string;
  push_event_commit_message (*atd message *): string;
  push_event_commit_author (*atd author *): push_event_author;
  push_event_commit_distinct (*atd distinct *): bool
}

type push_event_base = {
  push_event_base_ref (*atd ref *): string;
  push_event_base_before (*atd before *): string
}

type push_event = {
  push_event_head (*atd head *): string;
  push_event_size (*atd size *): int;
  push_event_commits (*atd commits *): push_event_commit list;
  push_event_ref (*atd ref *): string;
  push_event_before (*atd before *): string
}

type link = { href: string }

type pull_links = {
  pull_self (*atd self *): link;
  pull_html (*atd html *): link;
  pull_comments (*atd comments *): link;
  pull_review_comments (*atd review_comments *): link
}

type branch = {
  branch_label (*atd label *): string;
  branch_ref (*atd ref *): string;
  branch_sha (*atd sha *): string;
  branch_user (*atd user *): user option;
  branch_repo (*atd repo *): repository option
}

type pull = {
  pull_issue_url (*atd issue_url *): string;
  pull_number (*atd number *): int;
  pull_state (*atd state *): state;
  pull_title (*atd title *): string;
  pull_body (*atd body *): string;
  pull_created_at (*atd created_at *): string;
  pull_updated_at (*atd updated_at *): string;
  pull_closed_at (*atd closed_at *): string option;
  pull_merged_at (*atd merged_at *): string option;
  pull_head (*atd head *): branch;
  pull_base (*atd base *): branch;
  pull_links (*atd links *): pull_links;
  pull_user (*atd user *): user;
  pull_url (*atd url *): string;
  pull_html_url (*atd html_url *): string;
  pull_diff_url (*atd diff_url *): string;
  pull_patch_url (*atd patch_url *): string
}

type pulls = pull list

type body_changes = { body_changes_body (*atd body *): change option }

type pull_request_review_comment_action = [
    `Created
  | `Edited of body_changes
  | `Deleted
  | `Unknown of (string * json option)
]

type pull_request_review_comment = {
  pull_request_review_comment_diff_hunk (*atd diff_hunk *): string;
  pull_request_review_comment_original_position (*atd original_position *):
    int;
  pull_request_review_comment_original_commit_id (*atd original_commit_id *):
    string;
  pull_request_review_comment_pull_request_url (*atd pull_request_url *):
    string;
  pull_request_review_comment_position (*atd position *): int option;
  pull_request_review_comment_line (*atd line *): int option;
  pull_request_review_comment_path (*atd path *): string option;
  pull_request_review_comment_commit_id (*atd commit_id *): string;
  pull_request_review_comment_id (*atd id *): Int64.t;
  pull_request_review_comment_url (*atd url *): string;
  pull_request_review_comment_html_url (*atd html_url *): string;
  pull_request_review_comment_body (*atd body *): string;
  pull_request_review_comment_user (*atd user *): user;
  pull_request_review_comment_created_at (*atd created_at *): string;
  pull_request_review_comment_updated_at (*atd updated_at *): string
}

type pull_request_review_comment_event = {
  pull_request_review_comment_event_action (*atd action *):
    pull_request_review_comment_action;
  pull_request_review_comment_event_pull_request (*atd pull_request *): pull;
  pull_request_review_comment_event_comment (*atd comment *):
    pull_request_review_comment
}

type pull_request_action = [
    `Assigned
  | `Unassigned
  | `Labeled
  | `Unlabeled
  | `Opened
  | `Edited of ticket_changes
  | `Closed
  | `Reopened
  | `Synchronize
  | `Unknown of (string * json option)
]

type pull_request_event = {
  pull_request_event_action (*atd action *): pull_request_action;
  pull_request_event_number (*atd number *): int;
  pull_request_event_pull_request (*atd pull_request *): pull
}

type page_build_status = [
    `Building
  | `Built
  | `Errored
  | `Unknown of (string * json option)
]

type page_build_error = {
  page_build_error_message (*atd message *): string option
}

type page_build = {
  page_build_url (*atd url *): string;
  page_build_status (*atd status *): page_build_status option;
  page_build_error (*atd error *): page_build_error
}

type page_build_event = { page_build_event_build (*atd build *): page_build }

type orgs = org list

type organization = {
  organization_name (*atd name *): string;
  organization_company (*atd company *): string;
  organization_blog (*atd blog *): string;
  organization_location (*atd location *): string;
  organization_email (*atd email *): string;
  organization_public_repos (*atd public_repos *): int;
  organization_public_gists (*atd public_gists *): int;
  organization_followers (*atd followers *): int;
  organization_following (*atd following *): int;
  organization_html_url (*atd html_url *): string;
  organization_created_at (*atd created_at *): string;
  organization_login (*atd login *): string;
  organization_id (*atd id *): Int64.t;
  organization_url (*atd url *): string;
  organization_avatar_url (*atd avatar_url *): string option
}

type new_status = {
  new_status_state (*atd state *): status_state;
  new_status_target_url (*atd target_url *): string option;
  new_status_description (*atd description *): string option;
  new_status_context (*atd context *): string option
}

type new_repo = {
  new_repo_name (*atd name *): string;
  new_repo_description (*atd description *): string;
  new_repo_homepage (*atd homepage *): string;
  new_repo_private (*atd private *): bool;
  new_repo_has_issues (*atd has_issues *): bool;
  new_repo_has_wiki (*atd has_wiki *): bool;
  new_repo_has_downloads (*atd has_downloads *): bool;
  new_repo_team_id (*atd team_id *): int;
  new_repo_auto_init (*atd auto_init *): bool;
  new_repo_gitignore_template (*atd gitignore_template *): string option;
  new_repo_license_template (*atd license_template *): string option
}

type new_release = {
  new_release_tag_name (*atd tag_name *): string;
  new_release_target_commitish (*atd target_commitish *): string;
  new_release_name (*atd name *): string option;
  new_release_body (*atd body *): string option;
  new_release_draft (*atd draft *): bool;
  new_release_prerelease (*atd prerelease *): bool
}

type new_pull_issue = {
  new_pull_issue_issue (*atd issue *): int;
  new_pull_issue_base (*atd base *): string;
  new_pull_issue_head (*atd head *): string
}

type new_pull = {
  new_pull_title (*atd title *): string;
  new_pull_body (*atd body *): string option;
  new_pull_base (*atd base *): string;
  new_pull_head (*atd head *): string
}

type new_milestone = {
  new_milestone_title (*atd title *): string;
  new_milestone_state (*atd state *): state;
  new_milestone_description (*atd description *): string option;
  new_milestone_due_on (*atd due_on *): string option
}

type new_label = {
  new_label_name (*atd name *): string;
  new_label_color (*atd color *): string
}

type new_issue_comment = { new_issue_comment_body (*atd body *): string }

type new_issue = {
  new_issue_title (*atd title *): string;
  new_issue_body (*atd body *): string option;
  new_issue_assignee (*atd assignee *): string option;
  new_issue_milestone (*atd milestone *): int option;
  new_issue_labels (*atd labels *): string list
}

type new_hook = {
  new_hook_config (*atd config *): hook_config;
  new_hook_events (*atd events *): event_type list;
  new_hook_active (*atd active *): bool
}

type new_gist_content = { new_gist_content (*atd content *): string }

type new_gist_contents = (string * new_gist_content) list

type new_gist = {
  new_gist_files (*atd files *): new_gist_contents;
  new_gist_description (*atd description *): string;
  new_gist_public (*atd public *): bool
}

type new_deploy_key = {
  new_deploy_key_title (*atd title *): string;
  new_deploy_key_key (*atd key *): string
}

type milestones = milestone list

type milestone_sort = [ `Due_date | `Completeness ]

type error = {
  error_resource (*atd resource *): string;
  error_field (*atd field *): string option;
  error_code (*atd code *): string;
  error_message (*atd message *): string option
}

type message = {
  message_message (*atd message *): string;
  message_errors (*atd errors *): error list
}

type merge_request = {
  merge_commit_message (*atd commit_message *): string option
}

type merge = {
  merge_sha (*atd sha *): string option;
  merge_merged (*atd merged *): bool;
  merge_message (*atd message *): string
}

type member_action = [ `Added | `Unknown of (string * json option) ]

type member_event = {
  member_event_action (*atd action *): member_action;
  member_event_member (*atd member *): linked_user
}

type linked_users = linked_user list

type labels = label list

type label_names = string list

type issues_action = [
    `Assigned
  | `Unassigned
  | `Labeled
  | `Unlabeled
  | `Opened
  | `Edited of ticket_changes
  | `Closed
  | `Reopened
  | `Unknown of (string * json option)
]

type issues_event = {
  issues_event_action (*atd action *): issues_action;
  issues_event_issue (*atd issue *): issue;
  issues_event_assignee (*atd assignee *): user_info option;
  issues_event_label (*atd label *): label option
}

type issues = issue list

type issue_comment = {
  issue_comment_id (*atd id *): Int64.t;
  issue_comment_url (*atd url *): string;
  issue_comment_html_url (*atd html_url *): string;
  issue_comment_body (*atd body *): string;
  issue_comment_user (*atd user *): user;
  issue_comment_created_at (*atd created_at *): string;
  issue_comment_updated_at (*atd updated_at *): string
}

type issue_comments = issue_comment list

type issue_comment_action = [
    `Created
  | `Edited of body_changes
  | `Deleted
  | `Unknown of (string * json option)
]

type issue_comment_event = {
  issue_comment_event_action (*atd action *): issue_comment_action;
  issue_comment_event_issue (*atd issue *): issue;
  issue_comment_event_comment (*atd comment *): issue_comment
}

type hook = {
  hook_url (*atd url *): string;
  hook_updated_at (*atd updated_at *): string;
  hook_created_at (*atd created_at *): string;
  hook_events (*atd events *): event_type list;
  hook_active (*atd active *): bool;
  hook_config (*atd config *): hook_config;
  hook_id (*atd id *): Int64.t
}

type hooks = hook list

type gollum_event = { gollum_event_pages (*atd pages *): wiki_page list }

type git_ref = {
  git_ref_name (*atd name *): string;
  git_ref_url (*atd url *): string;
  git_ref_obj (*atd obj *): obj
}

type git_refs = git_ref list

type gist_fork = {
  gist_fork_user (*atd user *): user;
  gist_fork_url (*atd url *): string;
  gist_fork_id (*atd id *): Int64.t;
  gist_fork_created_at (*atd created_at *): string;
  gist_fork_updated_at (*atd updated_at *): string
}

type gist_file = {
  gist_file_size (*atd size *): int;
  gist_file_raw_url (*atd raw_url *): string;
  gist_file_ty (*atd ty *): string;
  gist_file_truncated (*atd truncated *): bool option;
  gist_file_language (*atd language *): string option;
  gist_file_content (*atd content *): string option
}

type gist_files = (string * gist_file) list

type change_status = {
  change_status_deletions (*atd deletions *): int;
  change_status_additions (*atd additions *): int;
  change_status_total (*atd total *): int
}

type gist_commit = {
  gist_commit_url (*atd url *): string;
  gist_commit_version (*atd version *): string;
  gist_commit_user (*atd user *): user;
  gist_commit_change_status (*atd change_status *): change_status;
  gist_commit_committed_at (*atd committed_at *): string
}

type gist_commits = gist_commit list

type gist = {
  gist_url (*atd url *): string;
  gist_forks_url (*atd forks_url *): string;
  gist_commits_url (*atd commits_url *): string;
  gist_id (*atd id *): string;
  gist_description (*atd description *): string option;
  gist_public (*atd public *): bool;
  gist_owner (*atd owner *): user;
  gist_user (*atd user *): string option;
  gist_files (*atd files *): gist_files;
  gist_comments (*atd comments *): int;
  gist_comments_url (*atd comments_url *): string;
  gist_html_url (*atd html_url *): string;
  gist_git_pull_url (*atd git_pull_url *): string;
  gist_git_push_url (*atd git_push_url *): string;
  gist_created_at (*atd created_at *): string;
  gist_updated_at (*atd updated_at *): string;
  gist_forks (*atd forks *): gist_fork list option;
  gist_history (*atd history *): gist_commits option
}

type gists = gist list

type gist_forks = gist_fork list

type fork_event = { fork_event_forkee (*atd forkee *): repository }

type file = {
  file_sha (*atd sha *): string option;
  file_filename (*atd filename *): string;
  file_status (*atd status *): string;
  file_additions (*atd additions *): int;
  file_deletions (*atd deletions *): int;
  file_changes (*atd changes *): int;
  file_blob_url (*atd blob_url *): string;
  file_raw_url (*atd raw_url *): string;
  file_patch (*atd patch *): string option
}

type files = file list

type delete_event = { delete_event_ref (*atd ref *): ref }

type create_event = {
  create_event_ref (*atd ref *): ref;
  create_event_master_branch (*atd master_branch *): string;
  create_event_description (*atd description *): string option
}

type commit_comment = {
  commit_comment_position (*atd position *): int option;
  commit_comment_line (*atd line *): int option;
  commit_comment_path (*atd path *): string option;
  commit_comment_commit_id (*atd commit_id *): string;
  commit_comment_id (*atd id *): Int64.t;
  commit_comment_url (*atd url *): string;
  commit_comment_html_url (*atd html_url *): string;
  commit_comment_body (*atd body *): string;
  commit_comment_user (*atd user *): user;
  commit_comment_created_at (*atd created_at *): string;
  commit_comment_updated_at (*atd updated_at *): string
}

type commit_comment_event = {
  commit_comment_event_comment (*atd comment *): commit_comment
}

type event_constr = [
    `CommitComment of commit_comment_event
  | `Create of create_event
  | `Delete of delete_event
  | `Download
  | `Follow
  | `Fork of fork_event
  | `ForkApply
  | `Gist
  | `Gollum of gollum_event
  | `IssueComment of issue_comment_event
  | `Issues of issues_event
  | `Member of member_event
  | `Public
  | `PullRequest of pull_request_event
  | `PullRequestReviewComment of pull_request_review_comment_event
  | `Push of push_event
  | `Release of release_event
  | `Repository of repository_event
  | `Status of status_event
  | `Watch of watch_event
  | `Unknown of (string * json option)
]

type event = {
  event_public (*atd public *): bool;
  event_payload (*atd payload *): event_constr;
  event_actor (*atd actor *): user;
  event_org (*atd org *): org option;
  event_created_at (*atd created_at *): string;
  event_repo (*atd repo *): repo;
  event_id (*atd id *): Int64.t
}

type events = event list

type event_hook_metadata = {
  event_hook_metadata_sender (*atd sender *): user;
  event_hook_metadata_organisation (*atd organisation *): org option;
  event_hook_metadata_created_at (*atd created_at *): string;
  event_hook_metadata_repository (*atd repository *): repository;
  event_hook_metadata_id (*atd id *): Int64.t
}

type event_hook_constr = [
    `CommitComment of commit_comment_event
  | `Create of create_event
  | `Delete of delete_event
  | `Download
  | `Follow
  | `Fork of fork_event
  | `ForkApply
  | `Gist
  | `Gollum of gollum_event
  | `IssueComment of issue_comment_event
  | `Issues of issues_event
  | `Member of member_event
  | `Public
  | `PullRequest of pull_request_event
  | `PullRequestReviewComment of pull_request_review_comment_event
  | `Push of push_event_hook
  | `Release of release_event
  | `Repository of repository_event
  | `Status of status_event
  | `Watch of watch_event
  | `Unknown of (string * json option)
]

type emojis = (string * string) list

type deploy_key = {
  deploy_key_id (*atd id *): Int64.t;
  deploy_key_key (*atd key *): string;
  deploy_key_url (*atd url *): string;
  deploy_key_title (*atd title *): string
}

type deploy_keys = deploy_key list

type contribution_week = {
  repo_contribution_week_w (*atd w *): int;
  repo_contribution_week_a (*atd a *): int;
  repo_contribution_week_d (*atd d *): int;
  repo_contribution_week_c (*atd c *): int
}

type contributor_stats = {
  repo_contributor_stats_author (*atd author *): user;
  repo_contributor_stats_total (*atd total *): int;
  repo_contributor_stats_weeks (*atd weeks *): contribution_week list
}

type contributors_stats = contributor_stats list

type contributor = {
  contributor_contributions (*atd contributions *): int;
  contributor_html_url (*atd html_url *): string;
  contributor_login (*atd login *): string;
  contributor_id (*atd id *): Int64.t;
  contributor_url (*atd url *): string;
  contributor_avatar_url (*atd avatar_url *): string option
}

type contributors = contributor list

type commits = commit list

type comment = {
  comment_id (*atd id *): Int64.t;
  comment_url (*atd url *): string;
  comment_html_url (*atd html_url *): string;
  comment_body (*atd body *): string;
  comment_user (*atd user *): user;
  comment_created_at (*atd created_at *): string;
  comment_updated_at (*atd updated_at *): string
}

type base_status = {
  base_status_url (*atd url *): string;
  base_status_updated_at (*atd updated_at *): string;
  base_status_created_at (*atd created_at *): string;
  base_status_id (*atd id *): Int64.t;
  base_status_state (*atd state *): status_state;
  base_status_target_url (*atd target_url *): string option;
  base_status_description (*atd description *): string option;
  base_status_context (*atd context *): string option
}

type base_statuses = base_status list

type combined_status = {
  combined_status_state (*atd state *): status_state;
  combined_status_sha (*atd sha *): string;
  combined_status_total_count (*atd total_count *): int;
  combined_status_statuses (*atd statuses *): base_statuses;
  combined_status_repository (*atd repository *): repo;
  combined_status_url (*atd url *): string;
  combined_status_commit_url (*atd commit_url *): string
}

type app = { app_name (*atd name *): string; app_url (*atd url *): string }

type auth = {
  auth_scopes (*atd scopes *): scope list;
  auth_token (*atd token *): string;
  auth_app (*atd app *): app;
  auth_url (*atd url *): string;
  auth_id (*atd id *): Int64.t;
  auth_note (*atd note *): string option;
  auth_note_url (*atd note_url *): string option
}

type auths = auth list

type auth_req = {
  auth_req_scopes (*atd scopes *): scope list;
  auth_req_note (*atd note *): string;
  auth_req_note_url (*atd note_url *): string option;
  auth_req_client_id (*atd client_id *): string option;
  auth_req_client_secret (*atd client_secret *): string option;
  auth_req_fingerprint (*atd fingerprint *): string option
}
